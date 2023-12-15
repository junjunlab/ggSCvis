translate_shape_string <- getFromNamespace("translate_shape_string", "ggplot2")
ggname <- getFromNamespace("ggname", "ggplot2")



#' Create a custom ggplot2 point geom
#'
#' This function creates a custom ggplot2 point geom.
#'
#' @param mapping Aesthetic mapping for the plot.
#' @param data A data frame containing the single-cell data.
#' @param stat The statistical transformation to apply (default is "identity").
#' @param position The positioning transformation to apply (default is "identity").
#' @param label.gp Cluster labels settings.
#' @param ... Additional arguments passed to the geom.
#' @param na.rm Should missing values be removed (default is FALSE).
#' @param show.legend Should the legend be displayed (default is NA).
#' @param inherit.aes Should aesthetics be inherited (default is TRUE).
#'
#' @return A custom ggplot2.
#' @export
geom_scPoint <- function(mapping = NULL, data = NULL,
                         stat = "identity", position = "identity",
                         ...,label.gp = NULL,
                         na.rm = FALSE,
                         show.legend = NA,
                         inherit.aes = TRUE) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSCpoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      label.gp = label.gp,
      ...
    )
  )
}




#' ggproto for GeomSCpoint
#' @format NULL
#' @usage NULL
#' @export
GeomSCpoint <- ggproto("GeomSCpoint", Geom,
                       required_aes = c("x", "y"),
                       non_missing_aes = c("size", "shape", "colour"),
                       default_aes = aes(
                         shape = 19, colour = "black", size = 1, fill = NA,
                         alpha = NA, stroke = 0.5,cluster = NULL,
                       ),

                       draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                                             label.gp = gpar()) {
                         if (is.character(data$shape)) {
                           data$shape <- translate_shape_string(data$shape)
                         }

                         coords <- coord$transform(data, panel_params)

                         # calculate cluster centers
                         if("cluster" %in% colnames(coords)){
                           centers <- coords %>%
                             dplyr::group_by(cluster) %>%
                             dplyr::summarise(x = median(x = x), y = median(x = y))

                           label_grob <- textGrob(x = centers$x,y = centers$y,
                                                  label = centers$cluster,
                                                  gp = label.gp,
                                                  default.units = "npc")
                         }else{
                           label_grob <- zeroGrob()
                         }

                         # =======================================================
                         # point grob
                         stroke_size <- coords$stroke
                         stroke_size[is.na(stroke_size)] <- 0

                         point_grob <- pointsGrob(
                           coords$x, coords$y,
                           pch = coords$shape,
                           default.units = "npc",
                           gp = gpar(
                             col = alpha(coords$colour, coords$alpha),
                             fill = alpha(coords$fill, coords$alpha),
                             # Stroke is added around the outside of the point
                             fontsize = coords$size * .pt + stroke_size * .stroke / 2,
                             lwd = coords$stroke * .stroke / 2
                           )
                         )

                         ggname("geom_scPoint",grid::gTree(children = gList(point_grob,label_grob)))
                       },

                       draw_key = draw_key_point
)
