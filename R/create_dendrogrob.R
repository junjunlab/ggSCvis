#' Create a Dendrogram-based Grob
#'
#' This function creates a dendrogram-based graphical object (Grob) with optional
#' vertical and horizontal dendrograms on the plot, along with associated axis labels.
#'
#' @param data A data frame containing the input data.
#' @param panel_params A list of panel parameters.
#' @param exp_mat Matrix data with values.
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
#' @return A list of graphical objects including the vertical and horizontal dendrograms
#' and their associated axis labels.
#'
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' @import ggdendro
#' @import grid
#' @importFrom stats as.dendrogram dist hclust order.dendrogram
create_dendrogrob <- function(data = NULL,
                              panel_params = NULL,
                              exp_mat = NULL,
                              tree.type = c("rectangle","triangle"),
                              add.tree.y = FALSE,
                              tree.y.side = "left",
                              tree.y.width = 0.06,
                              add.tree.x = FALSE,
                              tree.x.side = "bottom",
                              tree.x.height = 0.06,
                              tree.y.label.hjust = 1,
                              tree.x.label.hjust = 1,
                              tree.x.shift = 0,
                              tree.y.shift = 0,
                              new.ylabel.x = -0.05,
                              new.ylabel.width = 0.025,
                              new.ylabel.size = 8,
                              new.ylabel.face = "plain",
                              new.ylabel.rot = 0,
                              new.xlabel.y = -0.05,
                              new.xlabel.height = 0.025,
                              new.xlabel.size = 8,
                              new.xlabel.face = "plain",
                              new.xlabel.rot = 90){
  tree.type <- match.arg(tree.type,c("rectangle","triangle"))

  # long to matrix
  if(is.null(exp_mat)){
    exp_mat <- data %>%
      dplyr::select(x,y,exp) %>%
      tidyr::spread(key = x,value = exp) %>%
      tibble::column_to_rownames(var = "y")
  }else{
    exp_mat <- exp_mat
  }

  # =====================================================

  # add y axis tree
  if(add.tree.y == TRUE){
    # hclust for row
    dendy = as.dendrogram(hclust(dist(exp_mat)))

    # get tree data
    dend_dfy <- ggdendro::dendro_data(model = dendy,type = tree.type)$segments
    dend_dfy$y1 <- max(dend_dfy$y,dend_dfy$yend) - dend_dfy$yend
    dend_dfy$yend1 <- max(dend_dfy$y,dend_dfy$yend) - dend_dfy$y
    dend_dfy$x1 <- max(dend_dfy$x,dend_dfy$xend) - dend_dfy$xend + 1
    dend_dfy$xend1 <- max(dend_dfy$x,dend_dfy$xend) - dend_dfy$x + 1

    # check side
    if(tree.y.side == "right"){
      tree.y.x0 = dend_dfy$y
      tree.y.x1 = dend_dfy$yend
      tree.y.y0 = dend_dfy$x1
      tree.y.y1 = dend_dfy$xend1

      tree.y.just = "left"
      tree.y.x = 1 + tree.x.shift
      tree.y.label.hjust = 1
    }else{
      tree.y.x0 = dend_dfy$y1
      tree.y.x1 = dend_dfy$yend1
      tree.y.y0 = dend_dfy$x1
      tree.y.y1 = dend_dfy$xend1

      tree.y.just = "right"
      tree.y.x = 0 - tree.x.shift
      tree.y.label.hjust = 0
    }

    tree.groby <- segmentsGrob(x0 = tree.y.x0,x1 = tree.y.x1,
                               y0 = tree.y.y0,y1 = tree.y.y1,
                               default.units = "native",
                               vp = viewport(width = tree.y.width,height = 1,
                                             xscale = range(dend_dfy$y,dend_dfy$yend),
                                             yscale = panel_params$y.range,
                                             x = tree.y.x,y = 0.5,just = tree.y.just))

    # reorder data
    order_y <- order.dendrogram(dendy)
    yl <- unique(data$y)
    lapply(seq_along(yl), function(ii){
      tmp <- subset(data,y %in% yl[ii])
      idx <- match(yl[ii],order_y)
      tmp$y <- idx
      return(tmp)
    }) %>% Reduce("rbind",.) -> data


    # re-order coord y limit according to hclust order
    raw_limits <- panel_params$y$limits
    new_limits <- raw_limits[order.dendrogram(dendy)]

    yaxis_grob <- textGrob(label = new_limits,
                           x = 0,y = 1:length(new_limits),
                           gp = gpar(fontsize = new.ylabel.size,
                                     fontface = new.ylabel.face),
                           rot = new.ylabel.rot,
                           hjust = tree.y.label.hjust,
                           default.units = "native",
                           vp = viewport(width = 0.025,height = 1,
                                         xscale = c(0,1),
                                         yscale = panel_params$y.range,
                                         x = new.ylabel.x,y = 0.5))
  }else{
    tree.groby <- zeroGrob()
    yaxis_grob <- zeroGrob()
  }

  # =============================================================================
  # add x axis tree
  if(add.tree.x == TRUE){
    # hclust for row
    dendx = as.dendrogram(hclust(dist(t(exp_mat))))

    # get tree data
    dend_dfx <- ggdendro::dendro_data(model = dendx,type = tree.type)$segments
    dend_dfx$y1 <- max(dend_dfx$y,dend_dfx$yend) - dend_dfx$yend
    dend_dfx$yend1 <- max(dend_dfx$y,dend_dfx$yend) - dend_dfx$y
    dend_dfx$x1 <- max(dend_dfx$x,dend_dfx$xend) - dend_dfx$xend + 1
    dend_dfx$xend1 <- max(dend_dfx$x,dend_dfx$xend) - dend_dfx$x + 1

    # check side
    if(tree.x.side == "top"){
      tree.x.x0 = dend_dfx$x1
      tree.x.x1 = dend_dfx$xend1
      tree.x.y0 = dend_dfx$y
      tree.x.y1 = dend_dfx$yend

      tree.x.just = "bottom"
      tree.x.y = 1 + tree.y.shift

      tree.x.label.hjust = 1
    }else{
      tree.x.x0 = dend_dfx$x
      tree.x.x1 = dend_dfx$xend
      tree.x.y0 = dend_dfx$y1
      tree.x.y1 = dend_dfx$yend1

      tree.x.just = "top"
      tree.x.y = 0 - tree.y.shift

      tree.x.label.hjust = 0
    }

    tree.grobx <- segmentsGrob(x0 = tree.x.x0,x1 = tree.x.x1,
                               y0 = tree.x.y0,y1 = tree.x.y1,
                               default.units = "native",
                               vp = viewport(width = 1,height = tree.x.height,
                                             xscale = panel_params$x.range,
                                             yscale = range(dend_dfx$y,dend_dfx$yend),
                                             x = 0.5,y = tree.x.y,just = tree.x.just))

    # reorder data
    order_x <- order.dendrogram(dendx)
    xl <- unique(data$x)
    lapply(seq_along(xl), function(ii){
      tmp <- subset(data,x %in% xl[ii])
      idx <- match(xl[ii],order_x)
      tmp$x <- idx
      return(tmp)
    }) %>% Reduce("rbind",.) -> data


    # re-order coord y limit according to hclust order
    raw_limits <- panel_params$x$limits
    new_limits <- raw_limits[order.dendrogram(dendx)]


    xaxis_grob <- textGrob(label = new_limits,
                           x = 1:length(new_limits),y = 0,
                           gp = gpar(fontsize = new.xlabel.size,
                                     fontface = new.xlabel.face),
                           rot = new.xlabel.rot,
                           hjust = tree.x.label.hjust,
                           default.units = "native",
                           vp = viewport(width = 1,height = new.xlabel.height,
                                         xscale = panel_params$x.range,
                                         yscale = c(0,1),
                                         x = 0.5,y = new.xlabel.y))
  }else{
    tree.grobx <- zeroGrob()
    xaxis_grob <- zeroGrob()
  }

  # ============================================================================
  grobs <- list(data = data,
                tree.groby = tree.groby,
                yaxis_grob = yaxis_grob,
                tree.grobx = tree.grobx,
                xaxis_grob = xaxis_grob)
}
