modify_list <- getFromNamespace("modify_list", "ggplot2")
deprecate_soft0 <- getFromNamespace("deprecate_soft0", "ggplot2")
# wrap_as_facets_list <- getFromNamespace("wrap_as_facets_list","ggplot2")
check_number_whole <- getFromNamespace("check_number_whole","ggplot2")
empty <- getFromNamespace("empty","ggplot2")
eval_facets <- getFromNamespace("eval_facets","ggplot2")
as_unordered_factor <- getFromNamespace("as_unordered_factor","ggplot2")
join_keys <- getFromNamespace("join_keys","ggplot2")
check_facet_vars <- getFromNamespace("check_facet_vars","ggplot2")
id <- getFromNamespace("id","ggplot2")
# wrap_layout <- getFromNamespace("wrap_layout","ggplot2")
convertInd <- getFromNamespace("convertInd","ggplot2")
censor_labels <- getFromNamespace("censor_labels","ggplot2")
is.zero <- getFromNamespace("is.zero","ggplot2")
weave_axes <- getFromNamespace("weave_axes","ggplot2")
weave_tables_row <- getFromNamespace("weave_tables_row","ggplot2")
weave_tables_col <- getFromNamespace("weave_tables_col","ggplot2")
check_labeller <- getFromNamespace("check_labeller","ggplot2")
data_frame0 <- getFromNamespace("data_frame0","ggplot2")
grid_as_facets_list <- getFromNamespace("grid_as_facets_list","ggplot2")
df.grid <- getFromNamespace("df.grid","ggplot2")
reshape_add_margins <- getFromNamespace("reshape_add_margins","ggplot2")
unique0 <- getFromNamespace("unique0","ggplot2")
deprecate_warn0 <- getFromNamespace("deprecate_warn0","ggplot2")

as_facets_list <- getFromNamespace("as_facets_list","ggplot2")
compact_facets <- getFromNamespace("compact_facets","ggplot2")



wrap_layout <- function(id, dims, dir) {
  as.table <- TRUE
  n <- attr(id, "n")

  if (nchar(dir) != 2) {
    # Should only occur when `as.table` was not incorporated into `dir`
    dir <- switch(dir, h = "lt", v = "tl")
    deprecate_soft0(
      "3.5.2",
      what = I("Internal use of `dir = \"h\"` and `dir = \"v\"` in `facet_wrap()`"),
      details = I(c(
        "The `dir` argument should incorporate the `as.table` argument.",
        paste0("Falling back to `dir = \"", dir, "\"`.")
      ))
    )
  }

  dir <- arg_match0(dir, c("lt", "tl", "lb", "bl", "rt", "tr", "rb", "br"))

  ROW <- switch(
    dir,
    lt = , rt = (id - 1L) %/% dims[2] + 1L,
    tl = , tr = (id - 1L) %%  dims[1] + 1L,
    lb = , rb = dims[1] - (id - 1L) %/% dims[2],
    bl = , br = dims[1] - (id - 1L) %%  dims[1]
  )

  COL <- switch(
    dir,
    lt = , lb = (id - 1L) %% dims[2] + 1L,
    tl = , bl = (id - 1L) %/% dims[1] + 1L,
    rt = , rb = dims[2] - (id - 1L) %%  dims[2],
    tr = , br = dims[2] - (id - 1L) %/% dims[1]
  )

  data_frame0(
    PANEL = factor(id, levels = seq_len(n)),
    ROW   = as.integer(ROW),
    COL   = as.integer(COL),
    .size = length(id)
  )
}


wrap_as_facets_list <- function(x) {
  facets_list <- as_facets_list(x)
  compact_facets(facets_list)
}
