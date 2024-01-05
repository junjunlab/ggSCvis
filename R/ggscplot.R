#' Create a ggplot for single-cell data visualization
#'
#' This function creates a ggplot object for visualizing single-cell data which extract relatted
#' data from Seurat object before plotting.
#'
#' @param data A data frame containing the single-cell data.
#' @param mapping Aesthetic mapping for the plot.
#' @param object A Seurat object (optional) from which to fetch data.
#' @param reduction The reduction method to use (default is "umap").
#' @param features A vector of features to include in the plot (optional).
#' @param featuresAnno Annotation level for features (default is 0).
#' @param pct.exp.var A variable for percentage of expression (default is "seurat_clusters").
#' @param slot The data slot to use from the Seurat object (default is "data").
#' @param environment The levels of the gene name.
#' @param gene.order Character vectors to reorder the gene labels.
#' @param t the top margin of the plot (default is 0.1 npc).
#' @param r the right margin of the plot (default is 0.1 npc).
#' @param b the bottom margin of the plot (default is 0.1 npc).
#' @param l the left margin of the plot (default is 0.1 npc).
#' @param ... Additional arguments passed to ggplot.
#'
#' @return A ggplot object.
#' @export
#' @seealso
#' \code{\link{fetch_data}}
#'
#' @family ggscplot functions
#'
#' @importFrom stats median
#'
#' @rdname ggscplot
ggscplot <- function(data = NULL,
                     mapping = aes(),
                     object = NULL,
                     reduction = "umap",
                     features = NULL,
                     featuresAnno = 0,
                     pct.exp.var = "seurat_clusters",
                     slot = "data",
                     gene.order = NULL,
                     t = 0.1,r = 0.1,b = 0.1,l = 0.1,
                     environment = parent.frame(),
                     ...) {

  if(length(mapping) == 0){
    mapping <- aes(x = Dim1,y = Dim2)
  }

  if(!is.null(object)){
    data <- fetch_data(object = object,reduction = reduction,
                       features = features,featuresAnno = featuresAnno,
                       pct.exp.var = pct.exp.var,
                       slot = slot)

    # add levels
    if(!is.null(gene.order) & !is.null(features)){
      data$gene_name <- factor(data$gene_name,levels = gene.order)
    }
  }

  p <- ggplot(data = data,
              mapping = mapping,
              ...) +
    theme(plot.margin = margin(t = t,r = r,b = b,l = l,unit = "npc")) +
    coord_cartesian(clip = "off")

  class(p) <- c("ggscplot", class(p))

  return(p)
}
