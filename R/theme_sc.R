#' Customize ggplot2 theme for single-cell plots
#'
#' This function customizes the ggplot2 theme for single-cell plots by adjusting axis lines, labels, fonts, background color, and more.
#'
#' @param x.line.len Length of the x-axis arrow (default is 0.25).
#' @param x.label Label for the x-axis (default is "dim 1").
#' @param y.line.len Length of the y-axis arrow (default is 0.25).
#' @param y.label Label for the y-axis (default is "dim 1").
#' @param label.shift Shift for axis labels (default is -2.5).
#' @param fontsize Font size for labels (default is 10).
#' @param border.col Color of the plot border (default is NA).
#' @param add.arrow Should arrows be added to the axis lines? (default is TRUE).
#' @param ... Additional arguments passed to ggplot2 theme.
#' @param t the top margin of the plot (default is 0.1 npc).
#' @param r the right margin of the plot (default is 0.1 npc).
#' @param b the bottom margin of the plot (default is 0.1 npc).
#' @param l the left margin of the plot (default is 0.1 npc).
#' @param key.size legend size (default is 5).
#'
#' @return A ggplot2 theme object customized for single-cell plots.
#'
#' @export
theme_sc <- function(x.line.len = 0.25,x.label = "dim 1",
                     y.line.len = 0.25,y.label = "dim 1",
                     label.shift = -2.5,fontsize = 10,
                     border.col = NA,add.arrow = TRUE,
                     t = 0.1,r = 0.1,b = 0.1,l = 0.1,
                     key.size = 5,
                     ...) {
  if(add.arrow == TRUE){
    axis.line.x.bottom <- element_line2(x.line.ed = x.line.len,y.line.st = 1,
                                        label = x.label,label.shift = label.shift,
                                        fontsize = fontsize,
                                        arrow = arrow(length = unit(0.3,"cm"),
                                                      type = "closed",
                                                      ends = "last"))

    axis.line.y.left <- element_line2(y.line.ed = y.line.len,x.line.st = 1,
                                      label = y.label,label.shift = label.shift,
                                      fontsize = fontsize,
                                      arrow = arrow(length = unit(0.3,"cm"),
                                                    type = "closed",
                                                    ends = "last"))
  }else{
    axis.line.x.bottom <- element_blank()
    axis.line.y.left <- element_blank()
  }

  list(theme_grey(...) %+replace%
         theme(
           # axis.line = element_blank(),
           axis.line.x.bottom = axis.line.x.bottom,
           axis.line.y.left = axis.line.y.left,

           # white background and dark border
           panel.background = element_rect(fill = "white", colour = NA),
           panel.border     = element_rect(fill = NA, colour = border.col),
           # contour strips to match panel contour
           strip.background = element_rect(fill = "grey90", colour = "black"),
           strip.text = element_text(face = "bold.italic"),

           axis.ticks = element_blank(),
           axis.text = element_blank(),
           axis.title = element_blank(),
           panel.grid = element_blank(),
           plot.margin = margin(t = t,r = r,b = b,l = l,unit = "npc"),
           panel.spacing = unit(0.5,"cm"),

           complete = F
         ),
       guides(color = guide_legend(override.aes = list(size = key.size)))
       )

}
