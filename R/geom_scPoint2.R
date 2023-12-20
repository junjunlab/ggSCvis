#' Create a custom ggplot2 geom for single-cell data point plotting with labels and legend
#'
#' This function creates a custom ggplot2 geom for single-cell data point plotting with labels and optional legend.
#'
#' @param mapping Aesthetic mapping for the plot.
#' @param data A data frame containing the single-cell data.
#' @param stat The statistical transformation to apply (default is "identity").
#' @param position The positioning transformation to apply (default is "identity").
#' @param ... Additional arguments passed to the geom.
#' @param na.rm Should missing values be removed (default is FALSE).
#' @param show.legend Should the legend be displayed (default is NA).
#' @param inherit.aes Should aesthetics be inherited (default is TRUE).
#' @param label.gp A list of graphical parameters for labels (optional).
#' @param add_label Should labels be added to the points (default is TRUE).
#' @param add_legend Should a new legend be added (default is FALSE).
#' @param ncol Number of columns in the legend (default is 1).
#' @param vgap Vertical gap between legend items (default is 0.5).
#' @param hgap Horizontal gap between legend items (default is 1).
#' @param point.size Size of legend points (default is 2).
#' @param point.label.size Size of legend point labels (default is 10).
#' @param point.label.col Color of legend point labels (default is "black").
#' @param lgd_x X-position of the legend (default is 1.05).
#' @param lgd_y Y-position of the legend (default is 0.5).
#' @param lgd_width Width of the legend (default is 0.1).
#' @param lgd_height Height of the legend (default is 0.9).
#' @param fontsize Font size for labels and legend (default is 10).
#' @param fontface Font face for labels and legend (default is "plain").
#'
#' @return A custom ggplot2 geom for single-cell data point plotting with labels and legend.
#' @export
geom_scPoint2 <- function(mapping = NULL, data = NULL,
                          stat = "identity", position = "identity",
                          ...,
                          na.rm = FALSE,
                          show.legend = NA,
                          inherit.aes = TRUE,
                          label.gp = gpar(),add_label = TRUE,
                          add_legend = FALSE,
                          ncol = 1,vgap = 0.5,hgap = 1,
                          point.size = 2,point.label.size = 10,
                          point.label.col = "black",
                          lgd_x = 1.05,lgd_y = 0.5,
                          lgd_width = 0.1,lgd_height = 0.9,
                          fontsize = 10,fontface = "plain") {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSCpoint2,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      label.gp = label.gp,add_label = add_label,
      add_legend = add_legend,
      ncol = ncol,vgap = vgap,hgap = hgap,
      point.size = point.size,point.label.size = point.label.size,
      point.label.col = point.label.col,
      lgd_x = lgd_x,lgd_y = lgd_y,
      lgd_width = lgd_width,lgd_height = lgd_height,
      fontsize = fontsize,fontface = fontface,
      ...
    )
  )
}

#' ggproto for GeomSCpoint2
#' @format NULL
#' @usage NULL
#' @export
GeomSCpoint2 <- ggproto("GeomSCpoint2", Geom,
                        required_aes = c("x", "y"),
                        non_missing_aes = c("size", "shape", "colour"),
                        default_aes = aes(
                          shape = 19, colour = "black", size = 1, fill = "black",
                          alpha = NA, stroke = 0.5,cluster = NULL,cluster_anno = NULL
                        ),

                        draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                                              label.gp = gpar(),add_label = TRUE,
                                              add_legend = FALSE,
                                              ncol = 1,vgap = 0.5,hgap = 1,
                                              point.size = 2,point.label.size = 10,
                                              point.label.col = "black",
                                              lgd_x = 1.05,lgd_y = 0.5,
                                              lgd_width = 0.1,lgd_height = 0.9,
                                              fontsize = 10,fontface = "plain") {

                          # ==================================================================
                          if(add_legend == TRUE){
                            if(!("cluster_anno" %in% colnames(data) & "cluster_anno" %in% colnames(data))){
                              message("Please supply cluster_anno mapping variable.")
                              stop()
                            }

                            ld_df <- subset(data,select = c(cluster,cluster_anno,fill,colour,alpha)) %>%
                              unique() %>% dplyr::arrange(cluster_anno)

                            vp = viewport(x = lgd_x,y = lgd_y,just = "left",
                                          height = lgd_height,width = lgd_width)

                            legend_grob <- legendGrob2(labels = ld_df$cluster,
                                                       labels.point = ld_df$cluster_anno,
                                                       labels.point.gp = gpar(fontface = "bold",
                                                                              col = point.label.col,
                                                                              fontsize = point.label.size),
                                                       ncol = ncol,
                                                       pch = unique(data$shape),
                                                       vgap = unit(vgap, "lines"),
                                                       hgap = unit(hgap, "lines"),
                                                       gp = gpar(fontsize = fontsize,
                                                                 fontface = fontface,
                                                                 col = alpha(ld_df$colour, ld_df$alpha),
                                                                 fill = alpha(ld_df$fill, ld_df$alpha)),
                                                       point.size = point.size,
                                                       vp = vp)

                            lgd <- legend_grob
                          }else{
                            lgd <- zeroGrob()
                          }

                          # ==================================================================

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

                            if(add_label == FALSE){
                              label_grob <- zeroGrob()
                            }
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

                          ggname("geom_scPoint2",
                                 grid::gTree(children = gList(point_grob,label_grob,
                                                              lgd)))
                        },

                        draw_key = draw_key_point
)
