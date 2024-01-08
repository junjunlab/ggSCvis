#' Custom ggplot2 geometry: Scatter Dot
#'
#' This custom ggplot2 geometry function, \code{geom_scDot}, allows you to create
#' scatter dot plots in ggplot2.
#'
#' @param mapping Aesthetic mapping.
#' @param data A data frame.
#' @param stat The statistical transformation to use (default is "identity").
#' @param position The position adjustment to use (default is "identity").
#' @param ... Additional arguments to be passed to \code{\link[ggplot2]{geom_point}}.
#' @param na.rm Logical value indicating whether to remove missing values (default is FALSE).
#' @param show.legend Logical value indicating whether to show a legend for the layer (default is NA).
#' @param inherit.aes Logical value indicating whether to inherit aesthetics from the plot (default is TRUE).
#'
#' @param add.tree Whether add tree around the plot (default is FALSE).
#' @param tree.type Type of dendrogram tree ("rectangle" or "triangle").
#' @param add.tree.y Whether to add a vertical dendrogram.
#' @param tree.y.side Side for the vertical dendrogram ("left" or "right").
#' @param tree.y.width Width of the vertical dendrogram.
#' @param add.tree.x Whether to add a horizontal dendrogram.
#' @param tree.x.side Side for the horizontal dendrogram ("bottom" or "top").
#' @param tree.x.height Height of the horizontal dendrogram.
#' @param tree.y.label.hjust Horizontal justification for y-axis labels (0 or 1).
#' @param tree.x.label.hjust Horizontal justification for x-axis labels (0 or 1).
#' @param tree.x.shift Shift for the dendrogram placement.
#' @param tree.y.shift Shift for the dendrogram placement.
#' @param new.ylabel.x X-coordinate for the new y-axis labels.
#' @param new.ylabel.width Width of the new y-axis labels.
#' @param new.ylabel.size Font size for the new y-axis labels.
#' @param new.ylabel.face Font face for the new y-axis labels.
#' @param new.ylabel.rot Rotation angle for the new y-axis labels.
#' @param new.xlabel.y Y-coordinate for the new x-axis labels.
#' @param new.xlabel.height Height of the new x-axis labels.
#' @param new.xlabel.size Font size for the new x-axis labels.
#' @param new.xlabel.face Font face for the new x-axis labels.
#' @param new.xlabel.rot Rotation angle for the new x-axis labels.
#'
#' @param branch.side The side of the annotation segment (one of "top", "right", "bottom", or "left").
#' @param branch.shift The shift of the annotation segment along its side.
#' @param branch.len The length of the annotation segment.
#' @param branch.height The height of the annotation segment.
#' @param branch.lwd The line width of the annotation segment.
#' @param branch.label.rot The angle of labels on the annotation segment.
#' @param branch.label.size The font size of labels on the annotation segment.
#' @param branch.label.shift The shift of labels from the annotation segment.
#'
#' @return A ggplot2 layer.
#'
#' @export
geom_scDot <- function(mapping = NULL, data = NULL,
                       stat = "identity", position = "identity",
                       ...,
                       na.rm = FALSE,
                       show.legend = NA,
                       inherit.aes = TRUE,
                       add.tree = FALSE,
                       tree.type = "rectangle",
                       add.tree.y = FALSE,
                       tree.y.side = "left",
                       tree.y.width = 0.06,
                       add.tree.x = FALSE,
                       tree.x.side = "bottom",
                       tree.x.height = 0.06,
                       tree.y.label.hjust = 1,
                       tree.x.label.hjust = 1,
                       new.ylabel.x = -0.08,
                       new.ylabel.width = 0.025,
                       new.ylabel.size = 8,
                       new.ylabel.face = "plain",
                       new.ylabel.rot = 0,
                       new.xlabel.y = -0.08,
                       new.xlabel.height = 0.025,
                       new.xlabel.size = 8,
                       new.xlabel.face = "plain",
                       new.xlabel.rot = 90,
                       tree.x.shift = 0,
                       tree.y.shift = 0,
                       # =======================
                       branch.side = "top",
                       branch.shift = 0,
                       branch.height = 0.05,
                       branch.len = 0.8,
                       branch.lwd = 0.5,
                       branch.label.rot = NULL,
                       branch.label.size = 10,
                       branch.label.shift = 0.2) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSCdot,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      na.rm = na.rm,
      add.tree = add.tree,
      tree.type = tree.type,
      add.tree.y = add.tree.y,
      tree.y.side = tree.y.side,
      tree.y.width = tree.y.width,
      add.tree.x = add.tree.x,
      tree.x.side = tree.x.side,
      tree.x.height = tree.x.height,
      tree.y.label.hjust = tree.y.label.hjust,
      tree.x.label.hjust = tree.x.label.hjust,
      new.ylabel.x = new.ylabel.x,
      new.ylabel.width = new.ylabel.width,
      new.ylabel.size = new.ylabel.size,
      new.ylabel.face = new.ylabel.face,
      new.ylabel.rot = new.ylabel.rot,
      new.xlabel.y = new.xlabel.y,
      new.xlabel.height = new.xlabel.height,
      new.xlabel.size = new.xlabel.size,
      new.xlabel.face = new.xlabel.face,
      new.xlabel.rot = new.xlabel.rot,
      tree.x.shift = tree.x.shift,
      tree.y.shift = tree.y.shift,
      # =======================
      branch.side = branch.side,
      branch.shift = branch.shift,
      branch.height = branch.height,
      branch.len = branch.len,
      branch.lwd = branch.lwd,
      branch.label.rot = branch.label.rot,
      branch.label.size = branch.label.size,
      branch.label.shift = branch.label.shift,
      ...
    )
  )
}




#' GeomSCdot
#' @format NULL
#' @usage NULL
#' @export
GeomSCdot <- ggproto("GeomSCdot", Geom,
                     required_aes = c("x", "y"),
                     non_missing_aes = c("size", "shape", "colour"),
                     default_aes = aes(
                       shape = 21, colour = "black", size = 5, fill = NA,
                       alpha = NA, stroke = 0,cluster = NULL,
                       exp = NULL,celltype = NULL,
                     ),

                     draw_panel = function(self, data, panel_params, coord, na.rm = FALSE,
                                           add.tree = FALSE,
                                           tree.type = "rectangle",
                                           add.tree.y = FALSE,
                                           tree.y.side = "left",
                                           tree.y.width = 0.06,
                                           add.tree.x = FALSE,
                                           tree.x.side = "bottom",
                                           tree.x.height = 0.06,
                                           tree.y.label.hjust = 1,
                                           tree.x.label.hjust = 1,
                                           new.ylabel.x = -0.08,
                                           new.ylabel.width = 0.025,
                                           new.ylabel.size = 8,
                                           new.ylabel.face = "plain",
                                           new.ylabel.rot = 0,
                                           new.xlabel.y = -0.08,
                                           new.xlabel.height = 0.025,
                                           new.xlabel.size = 8,
                                           new.xlabel.face = "plain",
                                           new.xlabel.rot = 90,
                                           tree.x.shift = 0,
                                           tree.y.shift = 0,
                                           # =======================
                                           branch.side = "top",
                                           branch.shift = 0,
                                           branch.height = 0.05,
                                           branch.len = 0.8,
                                           branch.lwd = 0.5,
                                           branch.label.rot = NULL,
                                           branch.label.size = 10,
                                           branch.label.shift = 0.2) {


                       if (is.character(data$shape)) {
                         data$shape <- translate_shape_string(data$shape)
                       }

                       # remove dulicate expressions
                       data <- unique(data)

                       # =====================================================
                       # add dendrogram to row or col
                       if(add.tree == TRUE){
                         grobs <- create_dendrogrob(data = data,
                                                    panel_params = panel_params,
                                                    tree.type = tree.type,
                                                    add.tree.y = add.tree.y,
                                                    tree.y.side = tree.y.side,
                                                    tree.y.width = tree.y.width,
                                                    add.tree.x = add.tree.x,
                                                    tree.x.side = tree.x.side,
                                                    tree.x.height = tree.x.height,
                                                    tree.y.label.hjust = tree.y.label.hjust,
                                                    tree.x.label.hjust = tree.x.label.hjust,
                                                    tree.x.shift = tree.x.shift,
                                                    tree.y.shift = tree.y.shift,
                                                    new.ylabel.x = new.ylabel.x,
                                                    new.ylabel.width = new.ylabel.width,
                                                    new.ylabel.size = new.ylabel.size,
                                                    new.ylabel.face = new.ylabel.face,
                                                    new.ylabel.rot = new.ylabel.rot,
                                                    new.xlabel.y = new.xlabel.y,
                                                    new.xlabel.height = new.xlabel.height,
                                                    new.xlabel.size = new.xlabel.size,
                                                    new.xlabel.face = new.xlabel.face,
                                                    new.xlabel.rot = new.xlabel.rot)

                         tree.groby <- grobs$tree.groby
                         yaxis_grob <- grobs$yaxis_grob
                         tree.grobx <- grobs$tree.grobx
                         xaxis_grob <- grobs$xaxis_grob

                         data <- grobs$data
                       }else{
                         tree.groby <- zeroGrob()
                         yaxis_grob <- zeroGrob()
                         tree.grobx <- zeroGrob()
                         xaxis_grob <- zeroGrob()
                       }

                       # =======================================================
                       # annotation for genes
                       if("celltype" %in% colnames(data)){
                         # generate celltype anno grobs
                         seg_grobs <- create_annosegment(data = data,
                                                         panel_params = panel_params,
                                                         branch.side = branch.side,
                                                         branch.shift = branch.shift,
                                                         branch.height = branch.height,
                                                         branch.len = branch.len,
                                                         branch.lwd = branch.lwd,
                                                         branch.label.rot = branch.label.rot,
                                                         branch.label.size = branch.label.size,
                                                         branch.label.shift = branch.label.shift)

                         h_seg <- seg_grobs$h_seg
                         v_seg <- seg_grobs$v_seg
                         label_grob <- seg_grobs$label_grob
                       }else{
                         h_seg <- zeroGrob()
                         v_seg <- zeroGrob()
                         label_grob <- zeroGrob()
                       }

                       # =====================================================
                       coords <- coord$transform(data, panel_params)
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

                       ggname("geom_scDot",grid::gTree(children = gList(point_grob,
                                                                        h_seg,v_seg,label_grob,
                                                                        tree.groby,yaxis_grob,
                                                                        tree.grobx,xaxis_grob)))
                     },

                     # legend
                     draw_key = function(data, params, size) {
                       if (is.null(data$shape)) {
                         data$shape <- 19
                       } else if (is.character(data$shape)) {
                         data$shape <- translate_shape_string(data$shape)
                       }

                       # NULL means the default stroke size, and NA means no stroke.
                       stroke_size <- data$stroke %||% 0.5
                       stroke_size[is.na(stroke_size)] <- 0

                       p_grob <- pointsGrob(0.25, 0.5,
                                            pch = data$shape,
                                            gp = gpar(
                                              col = alpha(data$colour %||% "black", data$alpha),
                                              fill = "grey50",
                                              fontsize = (data$size %||% 1.5) * .pt + stroke_size * .stroke / 2,
                                              lwd = stroke_size * .stroke / 2
                                            )
                       )

                       s_grob <- segmentsGrob(x0 = 0.75,x1 = 1,
                                              y0 = 0.5,y1 = 0.5,
                                              gp = gpar(col = "black"))

                       gTree(children = gList(p_grob,s_grob))
                     }
)
