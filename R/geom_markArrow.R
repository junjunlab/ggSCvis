#' geom_markArrow
#' @author Jun Zhang
#'
#' An extension of ggplot2's GeomPath that adds an arrowhead to the end of a line.
#'
#' @param mapping The aesthetic mappings for the layer.
#' @param data The data to be displayed in this layer.
#' @param stat The statistical transformation to use on the data.
#' @param position The method used to position the objects.
#' @param na.rm If \code{TRUE}, removes missing values from the data.
#' @param show.legend Logical. Should this layer be included in the legends? NA,
#' the default, includes if any aesthetics are mapped. FALSE never includes, and
#' TRUE always includes.
#' @param inherit.aes If \code{TRUE} (the default), the plot's aesthetics will
#' be inherited. Set to \code{FALSE} to create a completely new set of aesthetic
#' mappings.
#' @param lineend The line ending style of the path; one of "butt" (default),
#' "round", or "square".
#' @param linejoin The line join style of the path; one of "round" (default),
#' "mitre", or "bevel".
#' @param rel.pos A numeric value between 0 and 1 indicating where along the
#' length of the path the arrowhead should be placed.
#' @param rel.len A numeric value between 0 and 1 indicating the size of the
#' arrowhead relative to the length of the path.
#' @param arrow The arrowhead to use, created using grid::arrow() or grid::arrowGrob().
#' @param arrow.fill The fill color for the arrowhead.
#' @param colour The color of the line and arrowhead.
#' @param label A character vector of length 2 indicating the labels for the
#' start and end points of the line.
#' @param label.size The font size for the labels.
#' @param label.shift A numeric vector of length 2 indicating the x and y offsets
#' for the label positions.
#' @param fontface The font face for the labels.
#' @param tail.shift A numeric value indicating the offset for the start point
#' of the line along its direction.
#' @param corner.pos A character vector indicating the position of the label
#' corner relative to the path; one of "left_b", "right_b", "left_t", or "right_t".
#' @param facet.vars A list of variables to use for faceting.
#' @param ... description
#'
#' @return An object of class \code{GeomMarkArrow}.
#'
#' @import ggplot2 grid
#' @export
geom_markArrow <- function(mapping = NULL, data = NULL, stat = "identity",
                           position = "identity", na.rm = FALSE,
                           show.legend = NA, inherit.aes = FALSE,
                           lineend = "butt",linejoin = "round",
                           rel.pos = 0.1,rel.len = 0.3,
                           arrow = grid::arrow(type = "closed",length = grid::unit(0.4,"cm")),
                           arrow.fill = NULL,colour = "black",
                           label = c("UMAP1","UMAP2"),label.size = 3,label.shift = c(0.025,0.025),
                           fontface = "bold.italic",tail.shift = 0,corner.pos = "left_b",
                           facet.vars = NULL,
                           ...) {
  # construct data
  if(!is.null(facet.vars)){
    data = data.frame(facet.vars)
  }

  # construct layer
  ggplot2::layer(
    geom = GeomMarkArrow, mapping = mapping,
    data = data,
    stat = stat, position = position,
    show.legend = show.legend, inherit.aes = inherit.aes,
    params = list(na.rm = na.rm,
                  lineend = lineend,
                  linejoin = linejoin,
                  rel.pos = rel.pos,
                  rel.len = rel.len,
                  arrow.fill = arrow.fill,
                  arrow = arrow,
                  label = label,
                  label.size = label.size,
                  label.shift = label.shift,
                  fontface = fontface,
                  tail.shift = tail.shift,
                  corner.pos = corner.pos,
                  colour = colour,
                  ...)
  )
}


#' ggproto of GeomMarkArrow
#' @format NULL
#' @usage NULL
#' @export
GeomMarkArrow <- ggplot2::ggproto("GeomMarkArrow", ggplot2::Geom,
                                  # required_aes = c("x", "y"),
                                  non_missing_aes = c("linetype", "linewidth", "shape"),
                                  default_aes = aes(linewidth = 0.5, linetype = 1,
                                                    alpha = NA),
                                  draw_key = draw_key_path,
                                  draw_panel = function(data, panel_scales, coord,
                                                        rel.pos = 0.1,rel.len = 0.3,
                                                        arrow.fill = NULL,colour = "black",
                                                        arrow = grid::arrow(type = "closed",length = grid::unit(0.4,"cm")),
                                                        lineend = "butt", linejoin = "round",
                                                        label = c("UMAP1","UMAP2"),label.size = 3,
                                                        label.shift = c(0.025,0.025),
                                                        fontface = "bold.italic",tail.shift = 0,
                                                        corner.pos = "left_b") {

                                    ## Transform the data first
                                    coords <- coord$transform(data, panel_scales)

                                    # position
                                    if(corner.pos == "left_b"){
                                      x = c(rel.pos,rel.pos)
                                      xend = c(rel.pos + rel.len,rel.pos)
                                      y = c(rel.pos,rel.pos)
                                      yend = c(rel.pos,rel.pos + rel.len)
                                    }else if(corner.pos == "left_u"){
                                      x = c(rel.pos,rel.pos)
                                      xend = c(rel.pos + rel.len,rel.pos)
                                      y = 1 - c(rel.pos,rel.pos)
                                      yend = 1 - c(rel.pos,rel.pos + rel.len)
                                    }else if(corner.pos == "right_b"){
                                      x = 1 - c(rel.pos,rel.pos)
                                      xend = 1 - c(rel.pos + rel.len,rel.pos)
                                      y = c(rel.pos,rel.pos)
                                      yend = c(rel.pos,rel.pos + rel.len)
                                    }else if(corner.pos == "right_u"){
                                      x = 1 - c(rel.pos,rel.pos)
                                      xend = 1 - c(rel.pos + rel.len,rel.pos)
                                      y = 1 - c(rel.pos,rel.pos)
                                      yend = 1 - c(rel.pos,rel.pos + rel.len)
                                    }

                                    # Construct segment data
                                    coords <- data.frame(
                                      x = x,xend = xend,
                                      y = y,yend = yend,
                                      PANEL = 1,group = c(1:2),
                                      linewidth = unique(coords$linewidth),
                                      colour = rep(colour,2),
                                      linetype = unique(coords$linetype),
                                      alpha = unique(coords$alpha),
                                      label = label)

                                    # Construct grid grobs
                                    arrow.fill <- arrow.fill %||% coords$colour

                                    seg_grob <-
                                      grid::segmentsGrob(
                                        x0 = c(coords$x[1] - tail.shift,coords$x[2]),
                                        x1 = coords$xend,
                                        y0 = c(coords$y[1],coords$y[2] - tail.shift),
                                        y1 = coords$yend,
                                        gp = grid::gpar(
                                          col = ggplot2::alpha(coords$colour, coords$alpha),
                                          fill = ggplot2::alpha(arrow.fill, coords$alpha),
                                          lwd = coords$linewidth * .pt,
                                          lty = coords$linetype,
                                          lineend = lineend,
                                          linejoin = linejoin
                                        ),
                                        arrow = arrow)

                                    text_grob <-
                                      grid::textGrob(label = coords$label,
                                                     x = c(((coords$x + coords$xend)/2)[1],
                                                           ((coords$x + coords$xend)/2)[2] - label.shift[2]),
                                                     y = c(((coords$y + coords$yend)/2)[1] - label.shift[1],
                                                           ((coords$y + coords$yend)/2)[2]),
                                                     hjust = 0.5,
                                                     rot = c(0,90),
                                                     check.overlap = TRUE,
                                                     gp = grid::gpar(col = ggplot2::alpha(coords$colour, coords$alpha),
                                                                     fontsize = label.size*.pt,
                                                                     fontface = fontface))

                                    # return
                                    grobs <- grid::grobTree(seg_grob,text_grob)
                                    grid::gTree(children = grid::gList(grobs))
                                  })
