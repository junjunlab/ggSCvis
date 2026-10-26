# =============================================================================
# Internal compatibility helpers
#
# ggSCvis reuses several internal (non-exported) helpers from ggplot2 and
# Seurat/SeuratObject. Some of them were renamed or removed in ggplot2 4.0.0
# (S7 refactor) and SeuratObject 5.0.0, so instead of capturing them once at
# build time they are resolved lazily at call time. This keeps the package
# working across ggplot2 3.5.x / 4.x and Seurat v4 / v5.
# =============================================================================

#' Get an internal object from another namespace, or NULL when absent
#' @noRd
.ggsc_get_fun <- function(pkg, name) {
  ns <- tryCatch(asNamespace(pkg), error = function(e) NULL)
  if (is.null(ns)) {
    return(NULL)
  }
  if (exists(name, envir = ns, inherits = FALSE)) {
    get(name, envir = ns)
  } else {
    NULL
  }
}

#' Is the installed ggplot2 at least `version`?
#' @noRd
.ggsc_gg_ge <- function(version) {
  utils::packageVersion("ggplot2") >= version
}

#' Weave axes into a panel gtable (ggplot2 3.5.x / 4.x compatible)
#'
#' ggplot2 < 4.0.0 returns `list(panels = <gtable>, sizes = <list>)`, while
#' ggplot2 >= 4.0.0 returns the gtable directly. This wrapper always returns
#' the `list(panels, sizes)` form that the facet code relies on; the axis sizes
#' are recomputed with the same `measure_axes()` helper ggplot2 4.x uses
#' internally.
#' @noRd
weave_axes <- function(panels, axes, empty = NULL) {
  fun <- .ggsc_get_fun("ggplot2", "weave_axes")
  if (is.null(fun)) {
    stop("ggplot2::weave_axes() is not available")
  }
  out <- fun(panels, axes = axes, empty = empty)

  if (!inherits(out, "gtable") && is.list(out) &&
      all(c("panels", "sizes") %in% names(out))) {
    return(out)
  }

  sizes <- tryCatch({
    measure_axes <- .ggsc_get_fun("ggplot2", "measure_axes")
    if (is.null(measure_axes)) {
      stop("measure_axes() is not available")
    }
    empty_idx <- which(empty %||% matrix(logical(), 0, 0), arr.ind = TRUE)
    sides <- match(names(axes), c("top", "right", "bottom", "left"))
    margin <- c(1L, 2L, 1L, 2L)[sides]
    shift <- c(1L, -1L, -1L, 1L)[sides]
    Map(
      measure_axes,
      axis = axes,
      margin = margin,
      shift = shift,
      MoreArgs = list(empty_idx = empty_idx)
    )
  }, error = function(e) NULL)

  if (is.null(sizes)) {
    sizes <- lapply(axes, function(x) unit(0, "cm"))
  }

  list(panels = out, sizes = sizes)
}

#' Test whether a grob has zero size
#'
#' ggplot2 < 4.0.0 provides the internal `is.zero()` generic, ggplot2 >= 4.0.0
#' replaced it with `is_zero()`. Both are looked up at call time.
#' @noRd
is.zero <- function(x) {
  fun <- .ggsc_get_fun("ggplot2", "is.zero") %||%
    .ggsc_get_fun("ggplot2", "is_zero")
  if (!is.null(fun)) {
    return(fun(x))
  }
  is.null(x) || inherits(x, "zeroGrob")
}

#' Validate a labeller
#'
#' ggplot2 < 4.0.0 provides the internal `check_labeller()`, ggplot2 >= 4.0.0
#' replaced it with `validate_labeller()`. Both are looked up at call time.
#' @noRd
check_labeller <- function(labeller) {
  fun <- .ggsc_get_fun("ggplot2", "check_labeller") %||%
    .ggsc_get_fun("ggplot2", "validate_labeller")
  if (!is.null(fun)) {
    return(fun(labeller))
  }
  match.fun(labeller)
}

#' Does the installed SeuratObject support `FetchData(layer = )`?
#'
#' SeuratObject >= 5.0.0 deprecated `slot` and introduced `layer`; the `slot`
#' argument is now defunct.
#' @noRd
.fetch_uses_layer <- function() {
  method <- tryCatch(
    utils::getS3method("FetchData", "Seurat", optional = TRUE),
    error = function(e) NULL
  )
  if (is.null(method)) {
    return(FALSE)
  }
  "layer" %in% names(formals(method))
}

#' Resolve a slot/layer request to an existing layer name
#'
#' Handles split `Assay5` layers (`counts.1`, `counts.2`, ...) by falling back
#' to the first layer whose name starts with the requested one.
#' @noRd
.resolve_layer <- function(object, slot) {
  assay <- tryCatch(object[[Seurat::DefaultAssay(object)]],
                    error = function(e) NULL)
  if (is.null(assay)) {
    return(slot)
  }
  lyr <- tryCatch(SeuratObject::Layers(assay), error = function(e) NULL)
  if (is.null(lyr) || !length(lyr)) {
    return(slot)
  }
  if (slot %in% lyr) {
    return(slot)
  }
  hit <- grep(paste0("^", slot, "([._].*)?$"), lyr, value = TRUE)
  if (length(hit)) {
    return(hit[[1L]])
  }
  slot
}

#' Fetch gene expression from a Seurat object (v4 / v5 compatible)
#' @noRd
.fetch_expression <- function(object, features, slot = "data") {
  if (.fetch_uses_layer()) {
    Seurat::FetchData(
      object = object,
      vars = features,
      layer = .resolve_layer(object, slot)
    )
  } else {
    Seurat::FetchData(
      object = object,
      vars = features,
      slot = slot
    )
  }
}
