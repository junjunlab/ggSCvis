#' Custom ggplot2 geometry: heatmap plot
#'
#' This custom ggplot2 geometry function, \code{geom_scDot}, allows you to create
#' heatmap plots in ggplot2.
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
#' @param branch.label.size The font size of labels on the annotation segment.
#' @param branch.label.shift The shift of labels from the annotation segment.
#' @param linejoin linejoin
#'
#' @return A ggplot2 layer.
#'
#' @export
geom_scTile <- function(mapping = NULL, data = NULL,
                        stat = "identity", position = "identity",
                        ...,
                        linejoin = "mitre",
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
                        branch.label.size = 10,
                        branch.label.shift = 0.2) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSCtile,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = rlang::list2(
      linejoin = linejoin,
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
      branch.label.size = branch.label.size,
      branch.label.shift = branch.label.shift,
      ...
    )
  )
}




#' GeomSCtile
#' @format NULL
#' @usage NULL
#' @export
GeomSCtile <- ggproto("GeomSCtile", ggplot2::GeomRect,
                      required_aes = c("x", "y"),

                      extra_params = c("na.rm"),

                      default_aes = aes(colour = NA, fill = "grey35", linewidth = 0.5, linetype = 1,
                                        alpha = NA,
                                        cluster = NULL,exp = NULL,celltype = NULL),

                      # These aes columns are created by setup_data(). They need to be listed here so
                      # that GeomRect$handle_na() properly removes any bars that fall outside the defined
                      # limits, not just those for which x and y are outside the limits
                      non_missing_aes = c("xmin", "xmax", "ymin", "ymax"),

                      setup_data = function(data, params) {
                        data$width <- data$width %||% params$width %||% resolution(data$x, FALSE)
                        data$height <- data$height %||% params$height %||% resolution(data$y, FALSE)

                        transform(data,
                                  xmin = x - width / 2,  xmax = x + width / 2,  width = NULL,
                                  ymin = y - height / 2, ymax = y + height / 2, height = NULL
                        )
                      },

                      draw_panel = function(self, data, panel_params, coord,
                                            lineend = "butt", linejoin = "mitre",
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
                                            new.ylabel.x = -0,
                                            new.ylabel.width = 0.025,
                                            new.ylabel.size = 8,
                                            new.ylabel.face = "plain",
                                            new.ylabel.rot = 0,
                                            new.xlabel.y = -0,
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
                                            branch.label.size = 10,
                                            branch.label.shift = 0.2){
                        # =====================================================
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

                        # =========================================================================
                        coords <- coord$transform(data, panel_params)

                        rect_grob <- rectGrob(coords$xmin, coords$ymax,
                                              width = coords$xmax - coords$xmin,
                                              height = coords$ymax - coords$ymin,
                                              default.units = "native",
                                              just = c("left", "top"),
                                              gp = gpar(
                                                col = coords$colour,
                                                fill = fill_alpha(coords$fill, coords$alpha),
                                                lwd = coords$linewidth * .pt,
                                                lty = coords$linetype,
                                                linejoin = linejoin,
                                                lineend = lineend))

                        ggname("geom_scTile",grid::gTree(children = gList(rect_grob,
                                                                          h_seg,v_seg,label_grob,
                                                                          tree.groby,yaxis_grob,
                                                                          tree.grobx,xaxis_grob)))

                      },

                      draw_key = draw_key_polygon
)
