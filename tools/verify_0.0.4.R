# =============================================================================
# Verification script for ggSCvis 0.0.4
#   - Seurat v5 (Assay5) + ggplot2 4.x compatibility
#   - fetch_data(slot/layer), ggscplot, geom_scDot, facet_new, theme_sc
# =============================================================================
suppressPackageStartupMessages({
  library(Seurat)
  library(ggplot2)
  library(ggSCvis)
})

cat("== session ==\n")
cat("R           :", R.version.string, "\n")
cat("ggplot2     :", as.character(packageVersion("ggplot2")), "\n")
cat("Seurat      :", as.character(packageVersion("Seurat")), "\n")
cat("SeuratObject:", as.character(packageVersion("SeuratObject")), "\n")
cat("ggSCvis     :", as.character(packageVersion("ggSCvis")), "\n")

outdir <- file.path(tempdir(), "ggscvis_verify")
dir.create(outdir, showWarnings = FALSE, recursive = TRUE)

fail <- 0
ok <- function(label, expr) {
  res <- tryCatch({
    force(expr)
    TRUE
  }, error = function(e) {
    cat("  [FAIL] ", label, ": ", conditionMessage(e), "\n", sep = "")
    FALSE
  })
  if (res) cat("  [ok] ", label, "\n", sep = "")
  if (!res) fail <<- fail + 1
  invisible(res)
}

# ---------------------------------------------------------------------------
# 1. build a small Seurat v5 object (Assay5)
# ---------------------------------------------------------------------------
cat("\n== 1. build test object (Seurat v5 / Assay5) ==\n")
set.seed(42)
n_genes <- 120; n_cells <- 240
mat <- matrix(
  rpois(n_genes * n_cells, lambda = 2),
  nrow = n_genes, ncol = n_cells,
  dimnames = list(paste0("gene", seq_len(n_genes)),
                  paste0("cell", seq_len(n_cells)))
)
# give the data some real cluster structure so that clustering gives >1 group
grp <- rep(1:4, length.out = n_cells)
for (k in 1:4) {
  mat[k, grp == k] <- mat[k, grp == k] + 40
}
obj <- CreateSeuratObject(counts = mat)
obj <- NormalizeData(obj, verbose = FALSE)
obj <- FindVariableFeatures(obj, nfeatures = 60, verbose = FALSE)
obj <- ScaleData(obj, verbose = FALSE)
obj <- RunPCA(obj, npcs = 10, verbose = FALSE)
obj <- FindNeighbors(obj, dims = 1:10, verbose = FALSE)
obj <- FindClusters(obj, resolution = 0.5, verbose = FALSE)
obj <- RunUMAP(obj, dims = 1:10, verbose = FALSE)
# guarantee several groups so that dendrogram / facet checks have >= 2 levels
obj$seurat_clusters <- factor(rep(0:3, length.out = ncol(obj)))
obj$celltype <- paste0("CT", as.integer(obj$seurat_clusters))
cat("  assay class :", class(obj[["RNA"]])[1], "\n")
cat("  layers      :", paste(SeuratObject::Layers(obj[["RNA"]]), collapse = ", "), "\n")
cat("  clusters    :", paste(levels(obj$seurat_clusters), collapse = ", "), "\n")

# ---------------------------------------------------------------------------
# 2. fetch_data(): the `slot` -> `layer` fix (issue #6, problem 1)
# ---------------------------------------------------------------------------
cat("\n== 2. fetch_data() slot/layer ==\n")
cat("  .fetch_uses_layer() :", ggSCvis:::.fetch_uses_layer(), "\n")

genes <- c("gene1", "gene2", "gene3", "gene4")

df_data <- NULL
ok("fetch_data(slot = 'data')", {
  df_data <<- fetch_data(object = obj, reduction = "umap",
                         features = genes,
                         featuresAnno = c("A", "A", "B", "B"),
                         pct.exp.var = "seurat_clusters",
                         slot = "data")
  stopifnot(is.data.frame(df_data))
  stopifnot(all(c("gene_name", "value", "mean_exp", "pct", "featureAnno") %in% colnames(df_data)))
})

df_counts <- NULL
ok("fetch_data(slot = 'counts')", {
  df_counts <<- fetch_data(object = obj, reduction = "umap",
                           features = genes,
                           featuresAnno = c("A", "A", "B", "B"),
                           pct.exp.var = "seurat_clusters",
                           slot = "counts")
  stopifnot(is.data.frame(df_counts))
})

ok("slot='counts' really returns counts (not normalized data)", {
  # counts are integers, normalized data are not
  v_cnt <- df_counts$value
  v_dat <- df_data$value
  stopifnot(all(abs(v_cnt - round(v_cnt)) < 1e-8))
  stopifnot(!all(abs(v_dat - round(v_dat)) < 1e-8))
})

ok("fetch_data() without features returns reduction + meta", {
  d <- fetch_data(object = obj, reduction = "umap")
  stopifnot(all(c("Dim1", "Dim2") %in% colnames(d)))
})

ok("fetch_data(group.vars = ) still works", {
  d <- fetch_data(object = obj, reduction = "umap",
                  features = genes[1:2],
                  group.vars = "celltype",
                  pct.exp.var = "seurat_clusters")
  stopifnot("group.vars" %in% colnames(d))
})

# ---------------------------------------------------------------------------
# 3. ggscplot + geom_scDot (issue #5) + facet_new
# ---------------------------------------------------------------------------
cat("\n== 3. ggscplot + geom_scDot + facet_new ==\n")

p_dot <- NULL
ok("ggscplot() + geom_scDot()", {
  p_dot <<- ggscplot(
    object = obj,
    mapping = aes(x = gene_name, y = celltype, fill = mean_exp, size = pct),
    features = genes,
    featuresAnno = c("A", "A", "B", "B")
  ) +
    geom_scDot() +
    scale_size(breaks = c(0, 20, 40, 60, 80, 100),
               limits = c(0, 100), range = c(0, 5))
})

ok("render geom_scDot to png", {
  ggsave(file.path(outdir, "scDot.png"), p_dot, width = 5, height = 4, dpi = 150)
})

ok("facet_new(facet_col = 'featureAnno')", {
  p <- p_dot +
    facet_new(facet_col = "featureAnno", scales = "free_x",
              x.angle = 90, x.label.hjust = 1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  ggsave(file.path(outdir, "scDot_facet.png"), p, width = 6, height = 4, dpi = 150)
})

ok("geom_scDot(add.tree = TRUE)", {
  p <- ggscplot(object = obj,
                mapping = aes(x = gene_name, y = celltype,
                              fill = mean_exp, size = pct, exp = mean_exp),
                features = genes) +
    geom_scDot(add.tree = TRUE, add.tree.y = TRUE, add.tree.x = TRUE)
  ggsave(file.path(outdir, "scDot_tree.png"), p, width = 6, height = 4, dpi = 150)
})

# ---------------------------------------------------------------------------
# 4. UMAP style plots
# ---------------------------------------------------------------------------
cat("\n== 4. UMAP / theme ==\n")
ok("ggscplot() UMAP + geom_scPoint()", {
  d <- fetch_data(object = obj, reduction = "umap")
  p <- ggscplot(data = d, mapping = aes(x = Dim1, y = Dim2, fill = celltype)) +
    geom_scPoint(size = 1, stroke = 0)
  ggsave(file.path(outdir, "umap.png"), p, width = 5, height = 4, dpi = 150)
})

ok("theme_sc() + element_line2 axis", {
  p <- ggplot(obj@meta.data, aes(x = celltype, y = "nCount", exp = nCount_RNA,
                                 fill = celltype)) +
    geom_scViolin() + theme_sc(x.label = "UMAP_1", y.label = "UMAP_2")
  ggsave(file.path(outdir, "violin.png"), p, width = 5, height = 4, dpi = 150)
})

# ---------------------------------------------------------------------------
# 5. facet_wrapnew / facet_gridnew (issue #6, problems 2 & 3)
# ---------------------------------------------------------------------------
cat("\n== 5. facet_wrapnew / facet_gridnew ==\n")
ok("facet_wrapnew()", {
  p <- ggplot(df_data, aes(x = gene_name, y = mean_exp, fill = gene_name)) +
    geom_scTile() +
    facet_wrapnew(~featureAnno, scales = "free_x")
  ggsave(file.path(outdir, "wrapnew.png"), p, width = 6, height = 4, dpi = 150)
})

ok("facet_gridnew()", {
  p <- ggplot(df_data, aes(x = gene_name, y = mean_exp, fill = mean_exp)) +
    geom_scTile() +
    facet_gridnew(rows = vars(featureAnno), cols = vars(celltype), scales = "free")
  ggsave(file.path(outdir, "gridnew.png"), p, width = 7, height = 5, dpi = 150)
})

ok("facet_hetamap()", {
  p <- ggplot(df_data, aes(x = gene_name, y = celltype, fill = mean_exp)) +
    geom_scTile() +
    facet_hetamap(facet_row = "celltype", facet_col = "featureAnno")
  ggsave(file.path(outdir, "hetamap.png"), p, width = 6, height = 4, dpi = 150)
})

# ---------------------------------------------------------------------------
# 6. issue #5: duplicated positions must not stack dots on top of each other
# ---------------------------------------------------------------------------
cat("\n== 6. issue #5 duplicated dots ==\n")
ok("duplicated (x, y) positions are collapsed with a warning", {
  obj2 <- obj
  # 4 clusters -> 2 celltypes, so pct is summarised per cluster (default
  # pct.exp.var) but plotted per celltype: several dots land on one position
  obj2$celltype <- factor(paste0("G", as.integer(obj$seurat_clusters) %/% 2))
  p <- ggscplot(object = obj2,
                mapping = aes(x = gene_name, y = celltype,
                              fill = mean_exp, size = pct),
                features = genes) +
    geom_scDot()
  w <- NULL
  gt <- withCallingHandlers(
    ggplot_gtable(ggplot_build(p)),
    warning = function(e) {
      w <<- conditionMessage(e)
      invokeRestart("muffleWarning")
    }
  )
  stopifnot(!is.null(w))
  stopifnot(grepl("position", w))
  ggsave(file.path(outdir, "scDot_dup.png"), p, width = 5, height = 4, dpi = 150)
})

# ---------------------------------------------------------------------------
# 7. ggplot2 4.0 internal helpers (issue #6, problems 2 & 3)
# ---------------------------------------------------------------------------
cat("\n== 7. internal helper shims ==\n")
ok("is.zero() shim works on zeroGrob / normal grob / NULL", {
  zg <- grid::nullGrob()                      # not a zeroGrob -> FALSE
  stopifnot(identical(ggSCvis:::is.zero(zg), FALSE))
  stopifnot(identical(ggSCvis:::is.zero(NULL), TRUE))
  zg2 <- get("zeroGrob", envir = asNamespace("ggplot2"))()
  stopifnot(identical(ggSCvis:::is.zero(zg2), TRUE))
})

ok("check_labeller() shim returns a function", {
  f <- ggSCvis:::check_labeller("label_value")
  stopifnot(is.function(f))
})

cat("\n== summary ==\n")
cat("output dir :", outdir, "\n")
cat("failures   :", fail, "\n")
if (fail > 0) quit(status = 1)
cat("ALL CHECKS PASSED\n")
