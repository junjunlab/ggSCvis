#' Create an annotation segment for dotplot.
#'
#' This function generates annotation segments for use in ggplot2 plots.
#' It allows you to add custom annotation segments to your plot.
#'
#' @param data A data frame containing the data for annotation segments.
#' @param panel_params A set of panel parameters (optional).
#' @param branch.side The side of the annotation segment (one of "top", "right", "bottom", or "left").
#' @param branch.shift The shift of the annotation segment along its side.
#' @param branch.len The length of the annotation segment.
#' @param branch.height The height of the annotation segment.
#' @param branch.lwd The line width of the annotation segment.
#' @param branch.label.rot The angle of labels on the annotation segment.
#' @param branch.label.hjust The hjust of labels on the annotation segment.
#' @param branch.label.size The font size of labels on the annotation segment.
#' @param branch.label.shift The shift of labels from the annotation segment.
#'
#' @return A list of graphical objects representing the annotation segment.
#'
#' @import ggplot2
#' @importFrom grid viewport segmentsGrob textGrob gpar
create_annosegment <- function(data = NULL,panel_params = NULL,
                               branch.side = c("top","right","bottom","left"),
                               branch.shift = 0,
                               branch.len = 0.9,
                               branch.height = 0.05,
                               branch.lwd = 0.5,
                               branch.label.rot = NULL,
                               branch.label.hjust = NULL,
                               branch.label.size = 10,
                               branch.label.shift = 0.2){
  branch.side <- match.arg(branch.side,c("top","right","bottom","left"))

  # generate celltype anno grobs
  cell_type <- unique(data$celltype)
  lapply(seq_along(cell_type), function(ii){

    if(branch.side %in% c("top","bottom")){
      tmp <- subset(data,celltype %in% cell_type[ii],select = c(x,celltype)) %>% unique()

      seg <- data.frame(x = min(tmp$x) - branch.len*0.5,
                        xend = max(tmp$x) + branch.len*0.5,
                        celltype = cell_type[ii])
    }else{
      tmp <- subset(data,celltype %in% cell_type[ii],select = c(y,celltype)) %>% unique()

      seg <- data.frame(x = min(tmp$y) - branch.len*0.5,
                        xend = max(tmp$y) + branch.len*0.5,
                        celltype = cell_type[ii])
    }


    return(seg)
  }) %>% Reduce("rbind",.) -> seg_df

  # ============================================================================
  # generate segments grob

  # check position
  if(branch.side == "top"){
    vp.width = 1
    vp.height = branch.height
    xscale = panel_params$x.range
    yscale = c(0,1)
    vp.x = 0.5
    vp.y = 1 + branch.shift
    vp.just = "bottom"

    x0 = seg_df$x
    x1 = seg_df$xend
    y0 = 1; y1 = 1

    vx0 = c(seg_df$x,seg_df$xend)
    vx1 = c(seg_df$x,seg_df$xend)
    vy0 = 0; vy1 = 1

    labelx = (x0 + x1)*0.5
    labely = vp.y + branch.label.shift
    label.rot = 90
    label.hjust = 0
  }else if(branch.side == "right"){
    vp.width = branch.height
    vp.height = 1
    xscale = c(0,1)
    yscale = panel_params$y.range
    vp.x = 1 + branch.shift
    vp.y = 0.5
    vp.just = "left"

    x0 = 1; x1 = 1
    y0 = seg_df$x
    y1 = seg_df$xend

    vx0 = 0; vx1 = 1
    vy0 = c(seg_df$x,seg_df$xend)
    vy1 = c(seg_df$x,seg_df$xend)

    labelx = vp.x + branch.label.shift
    labely = (y0 + y1)*0.5
    label.rot = 0
    label.hjust = 0
  }else if(branch.side == "bottom"){
    vp.width = 1
    vp.height = branch.height
    xscale = panel_params$x.range
    yscale = c(0,1)
    vp.x = 0.5
    vp.y = 0 - branch.shift
    vp.just = "top"

    x0 = seg_df$x
    x1 = seg_df$xend
    y0 = 0; y1 = 0

    vx0 = c(seg_df$x,seg_df$xend)
    vx1 = c(seg_df$x,seg_df$xend)
    vy0 = 0; vy1 = 1

    labelx = (x0 + x1)*0.5
    labely = vp.y - branch.label.shift
    label.rot = 90
    label.hjust = 1
  }else{
    vp.width = branch.height
    vp.height = 1
    xscale = c(0,1)
    yscale = panel_params$y.range
    vp.x = 0 - branch.shift
    vp.y = 0.5
    vp.just = "right"

    x0 = 0
    x1 = 0
    y0 = seg_df$x
    y1 = seg_df$xend

    vx0 = 0; vx1 = 1
    vy0 = c(seg_df$x,seg_df$xend)
    vy1 = c(seg_df$x,seg_df$xend)

    labelx = vp.x - branch.label.shift
    labely = (y0 + y1)*0.5
    label.rot = 0
    label.hjust = 1
  }

  vp <- viewport(width = vp.width,height = vp.height,
                 xscale = xscale,
                 yscale = yscale,
                 x = vp.x,y = vp.y,just = vp.just)

  h_seg <- segmentsGrob(x0 = x0,x1 = x1,
                        y0 = y0,y1 = y1,
                        gp = gpar(lwd = branch.lwd),
                        default.units = "native",
                        vp = vp)

  v_seg <- segmentsGrob(x0 = vx0,
                        x1 = vx1,
                        y0 = vy0,y1 = vy1,
                        gp = gpar(lwd = branch.lwd),
                        default.units = "native",
                        vp = vp)

  if(!is.null(branch.label.rot)) label.rot <- branch.label.rot
  if(!is.null(label.hjust)) label.hjust <- branch.label.hjust

  label_grob <- textGrob(label = seg_df$celltype,
                         x = labelx,
                         y = labely,
                         default.units = "native",
                         gp = gpar(fontsize = branch.label.size,
                                   fontface = "bold"),
                         rot = label.rot,hjust = label.hjust,
                         vp = vp)

  # ============================================================================
  grobs <- list(h_seg = h_seg,
                v_seg = v_seg,
                label_grob = label_grob)
}
