#' Custom ggplot2 geometry: violin plot
#'
#' This custom ggplot2 geometry function, \code{geom_scDot}, allows you to create
#' violin plots in ggplot2.
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
#' @param trim If TRUE (default), trim the tails of the violins to the range of the data.
#' If FALSE, don't trim the tails.
#'
#' @return A ggplot2 layer.
#'
#' @export
geom_scViolin <- function(mapping = NULL, data = NULL, stat = "identity",
                          position = "identity", show.legend = NA,
                          na.rm = FALSE,
                          inherit.aes = TRUE,
                          ...,
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
                          branch.label.shift = 0.2,
                          trim = TRUE) {
  ggplot2::layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomSCviolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
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
                  trim = trim,
                  ...)
  )
}


#' GeomSCviolin
#' @format NULL
#' @usage NULL
#' @export
GeomSCviolin <- ggplot2::ggproto("GeomSCviolin", ggplot2::Geom,
                                 required_aes = c("x", "y"),

                                 default_aes = aes(lwd = 0.5,width = 0.75,
                                                   colour = NA,fill = 'grey50',
                                                   alpha = 1,linetype = 1,angle = 0,
                                                   exp = NULL,cluster = NULL,celltype = NULL),

                                 draw_panel = function(self, data, panel_params, coord,
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
                                                       branch.label.shift = 0.2,
                                                       trim = TRUE) {

                                   # =====================================================
                                   # calculate density for each group
                                   groups_all <- unique(data$group)

                                   plyr::ldply(seq_along(groups_all), function(x){
                                     tmp <- data[which(data$group %in% groups_all[x]),]
                                     tmp_1 <- tmp[1,]

                                     density_df <- calc_density(data = tmp,trim = trim)
                                     # tmp_1 <- do.call("rbind", replicate(n = nrow(density_df), tmp_1, simplify = FALSE))
                                     tmp_1 <- plyr::rdply(nrow(density_df),tmp_1)


                                     tmp_1$vio_x <- density_df$vio_x
                                     tmp_1$vio_y <- density_df$vio_y
                                     tmp_1$meadian_exp <- density_df$meadian_exp

                                     return(tmp_1)
                                     # }) %>% Reduce("rbind",.) -> data
                                   }) -> data

                                   # =====================================================
                                   # long to wide
                                   exp_mat <- data %>%
                                     dplyr::select(x,y,meadian_exp) %>% unique() %>%
                                     tidyr::spread(key = x,value = meadian_exp) %>%
                                     tibble::column_to_rownames(var = "y")

                                   # =======================================================
                                   # add dendrogram to row or col
                                   if(add.tree == TRUE){
                                     grobs <- create_dendrogrob(data = data,
                                                                panel_params = panel_params,
                                                                exp_mat = exp_mat,
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
                                     uniq_data <- unique(data)
                                     # generate celltype anno grobs
                                     seg_grobs <- create_annosegment(data = uniq_data,
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

                                   # =================================================================================
                                   gp <- unique(data$group)

                                   # transform data
                                   coords <- coord$transform(data, panel_params) %>%
                                     dplyr::mutate(x.width = scales::rescale(width, from = panel_params$x.range)) %>%
                                     dplyr::mutate(y.width = scales::rescale(width, from = panel_params$y.range))

                                   # loop generate violin
                                   poly_list <- gList()
                                   for (x in seq_along(gp)){
                                     tmp <- coords[which(coords$group == gp[x]),]

                                     tmp <- tmp %>%
                                       dplyr::mutate(vio_x = scales::rescale(vio_x,to = c(0,1))) %>%
                                       dplyr::mutate(vio_y = scales::rescale(vio_y,to = c(0,1)))

                                     vp <- grid::viewport(x = unique(tmp$x),
                                                          y = unique(tmp$y),
                                                          width = unique(tmp$x.width),
                                                          height = unique(tmp$y.width),
                                                          angle = unique(tmp$angle),
                                                          just = c("center", "center"),
                                                          default.units = "native")

                                     # part to show
                                     polygen_xpos = c(0.5 - tmp$vio_x,rev(tmp$vio_x) + 0.5)
                                     polygen_ypos = c(tmp$vio_y,rev(tmp$vio_y))

                                     # get groups
                                     first_idx <- !duplicated(tmp$group)
                                     first_rows <- tmp[first_idx, ]

                                     # polygen
                                     poly_grob <- grid::polygonGrob(x = polygen_xpos,
                                                                    y = polygen_ypos,
                                                                    vp = vp,
                                                                    id = c(tmp$group,rev(tmp$group)),
                                                                    # name = unique(first_rows$group),
                                                                    gp = grid::gpar(col = ggplot2::alpha(first_rows$colour %||% "black", first_rows$alpha),
                                                                                    fill = ggplot2::alpha(first_rows$fill %||% "grey30", first_rows$alpha),
                                                                                    lwd = first_rows$size,
                                                                                    lty = first_rows$linetype))

                                     poly_list <- gList(poly_list,poly_grob)
                                   }

                                   ggplot2:::ggname("geom_scViolin",
                                                    gTree(children = gList(poly_list,
                                                                           h_seg,v_seg,label_grob,
                                                                           tree.groby,yaxis_grob,
                                                                           tree.grobx,xaxis_grob)))
                                 },

                                 # plot legend
                                 draw_key = draw_key_polygon
)
