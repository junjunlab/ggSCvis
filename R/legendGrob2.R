#' Create a custom legend grob (slightly modified from grid::legendGrob)
#'
#' This function creates a custom legend grob, allowing for
#' fine-grained control over the appearance of legend items.
#'
#' @param labels A list of labels for the legend items.
#' @param labels.point A list of labels for legend point items (optional).
#' @param labels.point.gp A list of graphical parameters for point labels (optional).
#' @param nrow Number of rows in the legend (optional).
#' @param ncol Number of columns in the legend (optional).
#' @param byrow Should legend items be arranged by row (default is FALSE).
#' @param do.lines Should lines be included in the legend (default is TRUE if 'lty' or 'lwd' are specified in 'gp', otherwise FALSE).
#' @param lines.first Should lines be placed before points in the legend (default is TRUE).
#' @param hgap Horizontal gap between legend items (default is 1 line).
#' @param vgap Vertical gap between legend items (default is 1 line).
#' @param default.units Default units for gaps (default is "lines").
#' @param point.size Size of legend points (default is 2).
#' @param pch A vector of point symbols (optional).
#' @param gp A list of graphical parameters for lines and borders (optional).
#' @param vp A grid viewport in which to draw the legend (optional).
#'
#' @importFrom grDevices as.graphicsAnnot
#'
#' @return A custom legend grob.
#' @export
legendGrob2 <- function(labels = NULL,
                        labels.point = NULL,
                        labels.point.gp = gpar(),
                        nrow = NULL, ncol = NULL, byrow = FALSE,
                        do.lines = has.lty || has.lwd, lines.first = TRUE,
                        hgap = unit(1, "lines"), vgap = unit(1, "lines"),
                        default.units = "lines",
                        point.size = 2,
                        pch = NULL, gp = gpar(), vp = NULL){
  # ============================================================================
  ## Type checking on arguments; labels: character, symbol or expression:
  labels <- as.graphicsAnnot(labels)
  labels <- if(is.character(labels)) as.list(labels) else as.expression(labels)
  nkeys <- if(is.call(labels)) 1 else length(labels)
  if(nkeys == 0) return(nullGrob(vp=vp))
  if (!is.unit(hgap))
    hgap <- unit(hgap, default.units)
  if (length(hgap) != 1) stop("'hgap' must be single unit")
  if (!is.unit(vgap))
    vgap <- unit(vgap, default.units)
  if (length(vgap) != 1) stop("'vgap' must be single unit")

  # ============================================================================
  ## nrow, ncol
  miss.nrow <- missing(nrow)
  miss.ncol <- missing(ncol)
  if(miss.nrow && miss.ncol) {ncol <- 1; nrow <- nkeys} # defaults to 1-column legend
  else if( miss.nrow && !miss.ncol) nrow <- ceiling(nkeys / ncol)
  else if(!miss.nrow &&  miss.ncol) ncol <- ceiling(nkeys / nrow)
  if(nrow < 1) stop("'nrow' must be >= 1")
  if(ncol < 1) stop("'ncol' must be >= 1")
  if(nrow * ncol < nkeys)
    stop("nrow * ncol < #{legend labels}")

  # ============================================================================
  ## pch, gp
  if(has.pch <- !missing(pch) && length(pch) > 0) pch <- rep_len(pch, nkeys)
  if(doGP <- length(nmgp <- names(gp)) > 0) {
    if(has.lty  <-  "lty" %in% nmgp) gp$lty  <- rep_len(gp$lty, nkeys)
    if(has.lwd  <-  "lwd" %in% nmgp) gp$lwd  <- rep_len(gp$lwd, nkeys)
    if(has.col  <-  "col" %in% nmgp) gp$col  <- rep_len(gp$col,  nkeys)
    if(has.fill <- "fill" %in% nmgp) gp$fill <- rep_len(gp$fill, nkeys)
  } else {
    gpi <- gp
    if(missing(do.lines)) do.lines <- FALSE
  }

  # ============================================================================
  ## main
  u0 <- unit(0, "npc")
  u1 <- unit(1, "char")
  ord <- if(lines.first) 1:2 else 2:1
  fg <- frameGrob(vp = vp)	  # set up basic frame grob (for packing)
  for (i in seq_len(nkeys)) {
    if(doGP) {
      gpi <- gp
      if(has.lty)	 gpi$lty <- gp$lty[i]
      if(has.lwd)	 gpi$lwd <- gp$lwd[i]
      if(has.col)	 gpi$col <- gp$col[i]
      if(has.fill) gpi$fill<- gp$fill[i]
    }
    if(byrow) {
      ci <- 1+ (i-1) %%  ncol
      ri <- 1+ (i-1) %/% ncol
    } else {
      ci <- 1+ (i-1) %/% nrow
      ri <- 1+ (i-1) %%  nrow
    }

    # ============================================================================
    ## borders; unit.c creates a 4-vector of borders (bottom, left, top, right)
    vg <- if(ri != nrow) vgap else u0
    symbol.border <- unit.c(vg, u0, u0, 0.5 * hgap)
    text.border   <- unit.c(vg, u0, u0, if(ci != ncol) hgap else u0)

    # ============================================================================
    ## points/lines grob:
    plGrob <- if(has.pch && do.lines){
      gTree(children = gList(linesGrob(0:1, 0.5, gp=gpi),
                             pointsGrob(0.5, 0.5, default.units="npc",
                                        size = unit(point.size, "char"),
                                        pch=pch[i], gp=gpi),
                             textGrob(label = labels.point[[i]],
                                      x = 0.5,y = 0.5,
                                      default.units="npc",
                                      gp = labels.point.gp))[ord])
    }else if(has.pch){
      gTree(children = gList(pointsGrob(0.5, 0.5,default.units="npc",
                                        size = unit(point.size, "char"),
                                        pch=pch[i], gp=gpi),
                             textGrob(label = labels.point[[i]],
                                      x = 0.5,y = 0.5,
                                      default.units="npc",
                                      gp = labels.point.gp)))
    }else if(do.lines){
      linesGrob(0:1, 0.5, gp=gpi)
    }else{
      nullGrob() # should not happen...
    }

    fg <- packGrob(fg, plGrob,
                   col = 2*ci-1, row = ri, border = symbol.border,
                   width = u1, height = u1, force.width = TRUE)

    # ============================================================================
    ## text grob: add the labels
    gpi. <- gpi
    gpi.$col <- "black" # maybe needs its own 'gp' in the long run (?)
    fg <- packGrob(fg, textGrob(labels[[i]], x = 0, y = 0.5,
                                just = c("left", "centre"), gp=gpi.),
                   col = 2*ci, row = ri, border = text.border)
  }
  fg
}
