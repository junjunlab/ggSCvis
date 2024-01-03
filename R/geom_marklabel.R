#' geom_marklabel
#'
#' Add labeled markers to a plot.
#'
#' This function adds labeled markers to a plot using the GeomMarklabel layer.
#'
#' @param mapping Aesthetic mapping.
#' @param data The data to be plotted.
#' @param stat The statistical transformation to be applied to the data.
#' @param position The position adjustment method for overlapping points.
#' @param ... Additional arguments to be passed to GeomMarklabel.
#' @param na.rm Logical. Should missing values be removed?
#' @param show.legend Should this layer be included in the legend?
#' @param inherit.aes Should inherit aesthetics from the parent plot?
#' @param mark.label The labels to be displayed with markers.
#' @param shift The shift of labels (0).
#' @param use.smartAlign2 Logical, whether to use smart alignment.
#' @param link.line.length Length of the connecting lines.
#' @param link.label.space Spacing between the label and connecting lines.
#' @param mark.scale A numeric vector specifying the scale for mark positioning.
#' @param link.label.gp A graphical parameter for the label appearance.
#' @param link.line.gp A graphical parameter for the connecting lines.
#' @param link.circle.start.gp A graphical parameter for the start circle (if used).
#' @param link.circle.end.gp A graphical parameter for the end circle (if used).
#' @param link.start.type Type of shape at the start of the connecting lines ("line", "circle", or "arrow").
#' @param link.end.type Type of shape at the end of the connecting lines ("line", "circle", or "arrow").
#' @param circle.arrow.size A numeric vector specifying the size of circles or arrows.
#' @param pos Alignment position ("right", "left", "top", or "bottom").
#'
#' @return A ggplot2 layer for labeled markers.
#'
#' @importFrom ggplot2 ggproto layer
#' @importFrom ggcirclize smartLabelAlignGrob
#'
#' @export
geom_marklabel <- function(mapping = NULL, data = NULL,
                           stat = "identity", position = "identity",
                           ...,
                           na.rm = FALSE,
                           show.legend = NA,
                           inherit.aes = TRUE,
                           mark.label = NULL,
                           shift = 0,
                           use.smartAlign2 = FALSE,
                           link.line.length = 0.075,
                           link.label.space = 0.025,
                           mark.scale = c(0,1),
                           link.label.gp = gpar(fontsize = 10),
                           link.line.gp = gpar(fill = "black",col = "black"),
                           link.circle.start.gp = gpar(fill = "black",col = "black"),
                           link.circle.end.gp = gpar(fill = "black",col = "black"),
                           link.start.type = c("line","circle","arrow"),
                           link.end.type = c("line","circle","arrow"),
                           circle.arrow.size = c(0.01,0.01),
                           pos = c("right","left","top","bottom")) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomMarklabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params =rlang::list2(
      na.rm = na.rm,
      mark.label = mark.label,
      shift = shift,
      use.smartAlign2 = use.smartAlign2,
      link.line.length = link.line.length,
      link.label.space = link.label.space,
      mark.scale = mark.scale,
      link.label.gp = link.label.gp,
      link.line.gp = link.line.gp,
      link.circle.start.gp = link.circle.start.gp,
      link.circle.end.gp = link.circle.end.gp,
      link.start.type = link.start.type,
      link.end.type = link.end.type,
      circle.arrow.size = circle.arrow.size,
      pos = pos,
      ...
    )
  )
}



#' GeomMarklabel
#' @format NULL
#' @usage NULL
#' @export
GeomMarklabel <- ggproto("GeomMarklabel", Geom,
                         required_aes = c("x", "y"),

                         draw_panel = function(self, data, panel_params, coord,
                                               mark.label = NULL,
                                               shift = 0,
                                               use.smartAlign2 = FALSE,
                                               link.line.length = 0.075,
                                               link.label.space = 0.025,
                                               mark.scale = c(0,1),
                                               link.label.gp = gpar(fontsize = 10),
                                               link.line.gp = gpar(fill = "black",col = "black"),
                                               link.circle.start.gp = gpar(fill = "black",col = "black"),
                                               link.circle.end.gp = gpar(fill = "black",col = "black"),
                                               link.start.type = c("line","circle","arrow"),
                                               link.end.type = c("line","circle","arrow"),
                                               circle.arrow.size = c(0.01,0.01),
                                               pos = c("right","left","top","bottom")){
                           pos <- match.arg(pos,c("right","left","top","bottom"))

                           # =====================================================
                           if(pos %in% c("right","left")){
                             all.label <- panel_params$y$limits
                             mark_label <- intersect(mark.label,all.label)

                             if(pos == "right"){
                               x = 1 + shift
                               y = 0.5
                             }else{
                               x = 0 - shift
                               y = 0.5
                             }
                           }else{
                             all.label <- panel_params$x$limits
                             mark_label <- intersect(mark.label,all.label)

                             if(pos == "top"){
                               y = 1 + shift
                               x = 0.5
                             }else{
                               y = 0 - shift
                               x = 0.5
                             }
                           }

                           # generate label grob
                           if(!is.null(mark.label) & length(mark_label) > 0){
                             label_grob <- ggcirclize::smartLabelAlignGrob(all.label = all.label,
                                                                           mark.label = mark_label,
                                                                           x = x,y = y,
                                                                           use.smartAlign2 = use.smartAlign2,
                                                                           link.line.length = link.line.length,
                                                                           link.label.space = link.label.space,
                                                                           mark.scale = mark.scale,
                                                                           link.label.gp = link.label.gp,
                                                                           link.line.gp = link.line.gp,
                                                                           link.circle.start.gp = link.circle.start.gp,
                                                                           link.circle.end.gp = link.circle.end.gp,
                                                                           link.start.type = link.start.type,
                                                                           link.end.type = link.end.type,
                                                                           circle.arrow.size = circle.arrow.size,
                                                                           pos = pos)
                           }else{
                             label_grob <- zeroGrob()
                           }

                           # =========================================================================

                           ggname("geom_marklabel",grid::gTree(children = gList(label_grob)))

                         },

                         draw_key = draw_key_blank
)
