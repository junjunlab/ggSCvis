len0_null <- getFromNamespace('len0_null', 'ggplot2')
modify_list <- getFromNamespace("modify_list", "ggplot2")
deprecate_soft0 <- getFromNamespace("deprecate_soft0", "ggplot2")



#' element_line2 control the axis line length
#'
#' @title element_line2
#'
#' @param linetype Line type. An integer (0:8), a name (blank, solid,
#'    dashed, dotted, dotdash, longdash, twodash), or a string with
#'    an even number (up to eight) of hexadecimal digits which give the
#'    lengths in consecutive positions in the string.
#' @param x.line.st x line start position default(0).
#' @param x.line.ed x line end position default(1).
#' @param y.line.st y line start position default(0).
#' @param y.line.ed y line end position default(1).
#' @param label.shift label distance to the line default(-0.1).
#' @param label labels around the line default("dim").
#' @param fontface label fontface default("bold.italic").
#' @param fontsize label fontsize default(8).
#' @param arrow Arrow specification, as created by [grid::arrow()]
#' @param colour colour
#' @param linewidth linewidth
#' @param lineend lineend
#' @param color color
#' @param inherit.blank inherit.blank
#' @param size size
#'
#' @import grid
#' @import ggplot2
#'
#' @export
element_line2 <- function(colour = NULL, linewidth = NULL, linetype = NULL,
                          lineend = NULL, color = NULL, arrow = NULL,
                          inherit.blank = FALSE, size = lifecycle::deprecated(),
                          x.line.st = 0,x.line.ed = 1,
                          y.line.st = 0,y.line.ed = 1,
                          label.shift = -0.1,label = "dim",
                          fontface = "bold.italic",fontsize = 8) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_line(size)", "element_line(linewidth)")
    linewidth <- size
  }

  if (!is.null(color))  colour <- color
  if (is.null(arrow)) arrow <- FALSE
  structure(
    list(colour = colour, linewidth = linewidth, linetype = linetype, lineend = lineend,
         arrow = arrow, inherit.blank = inherit.blank,
         x.line.st = x.line.st,x.line.ed = x.line.ed,
         y.line.st = y.line.st,y.line.ed = y.line.ed,
         label.shift = label.shift,label = label,
         fontface = fontface,fontsize = fontsize
    ),
    class = c("element_line2","element_line", "element")
  )
}




#' @importFrom ggplot2 element_grob
#' @method element_grob element_line2
#' @export
element_grob.element_line2 <- function(element,...,
                                       x = 0:1, y = 0:1,
                                       colour = NULL, linewidth = NULL, linetype = NULL, lineend = NULL,
                                       default.units = "npc", id.lengths = NULL, size = deprecated()) {

  if (lifecycle::is_present(size)) {
    deprecate_soft0("3.4.0", "element_grob.element_line(size)", "element_grob.element_line(linewidth)")
    linewidth <- size
  }

  # The gp settings can override element_gp
  gp <- gpar(
    col = colour, fill = colour,
    lwd = len0_null(linewidth * .pt), lty = linetype, lineend = lineend
  )
  element_gp <- gpar(
    col = element$colour, fill = element$colour,
    lwd = len0_null(element$linewidth * .pt), lty = element$linetype,
    lineend = element$lineend
  )
  arrow <- if (is.logical(element$arrow) && !element$arrow) {
    NULL
  } else {
    element$arrow
  }

  line.grob <- polylineGrob(
    x = c(element$x.line.st,element$x.line.ed),
    y = c(element$y.line.st,element$y.line.ed),
    # x = x,y = y,
    default.units = default.units,
    gp = modify_list(element_gp, gp),
    id.lengths = id.lengths, arrow = arrow
  )

  # ============================================================================
  # labels
  if(element$y.line.st == element$y.line.ed){
    label.x = (element$x.line.st + element$x.line.ed)*0.5
    label.y = element$label.shift
    rot = 0
  }else{
    label.y = (element$y.line.st + element$y.line.ed)*0.5
    label.x = element$label.shift
    rot = 90
  }

  label.grob <- textGrob(label = element$label,
                         x = label.x,y = label.y,
                         rot = rot,
                         gp = gpar(fontface = element$fontface,fontsize = element$fontsize),
                         default.units = default.units)

  gTree(children = gList(line.grob,label.grob))
}
