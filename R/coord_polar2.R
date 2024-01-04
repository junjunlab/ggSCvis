#' coord_polar2
#'
#' Create a polar coordinate system.
#'
#' This function creates a polar coordinate system for a ggplot2 plot.
#'
#' @param theta The variable to be mapped to the angle (x or y). Default is "x".
#' @param r0 The inner radius of the polar coordinate system. Default is 0.
#' @param r1 The outer radius of the polar coordinate system. Default is 1.
#' @param start The starting angle in degrees. Default is 0.
#' @param end The ending angle in degrees. Default is 360.
#' @param direction The direction of rotation (1 for counterclockwise, -1 for clockwise). Default is 1.
#' @param clip Clip points outside the polar coordinate system (on/off). Default is "off".
#' @param nice.facing Should facing labels be automatically adjusted for clarity? Default is FALSE.
#' @param x.label.rot Should x-axis labels be rotated? Default is FALSE.
#' @param x.label.pos The position of x-axis labels (top or bottom). Default is "top".
#' @param x.label.shift The shift of x-axis labels. Default is 0.01.
#' @param y.label.show Show y-axis labels (TRUE/FALSE). Default is FALSE.
#' @param y.label.pos The position of y-axis labels (left or right). Default is "left".
#' @param y.label.shift The shift of y-axis labels. Default is 1.
#'
#' @return A ggplot2 polar coordinate system.
#'
#' @import ggplot2
#' @import scales
#' @import rlang
#' @import vctrs
#' @importFrom ggcirclize as.radian as.theta
#'
#' @export
coord_polar2 <- function(theta = "x",
                         r0 = 0,r1 = 1,
                         start = 0,end = 360,
                         direction = 1, clip = "off",
                         nice.facing = FALSE,
                         x.label.rot = FALSE,
                         x.label.pos = c("top","bottom"),
                         x.label.shift = 0.01,
                         y.label.show = FALSE,
                         y.label.pos = c("left","right"),
                         y.label.shift = 1) {
  theta <- arg_match0(theta, c("x", "y"))
  x.label.pos <- match.arg(x.label.pos,c("top","bottom"))
  y.label.pos <- match.arg(y.label.pos,c("left","right"))

  r <- if (theta == "x") "y" else "x"

  ggproto(NULL, CoordPolar2,
          theta = theta,
          r = r,
          r0 = r0,
          r1 = r1,
          start = as.radian(start),
          end = as.radian(end),
          direction = sign(direction),
          clip = clip,
          nice.facing = nice.facing,
          x.label.rot = x.label.rot,
          x.label.pos = x.label.pos,
          x.label.shift = x.label.shift,
          y.label.show = y.label.show,
          y.label.pos = y.label.pos,
          y.label.shift = y.label.shift
  )
}




#' CoordPolar2
#' @format NULL
#' @usage NULL
#' @export
CoordPolar2 <- ggproto("CoordPolar2", Coord,

                       aspect = function(details) 1,

                       is_free = function() TRUE,

                       distance = function(self, x, y, details) {
                         # arc <- self$start + c(0, self$end)
                         arc <- c(self$start,self$end)

                         dir <- self$direction
                         if (self$theta == "x") {

                           r <- rescale(y, from = details$r.range, to = c(self$r0,self$r1))
                           theta <- theta_rescale_no_clip(x, details$theta.range, arc, dir)
                         } else {
                           r <- rescale(x, from = details$r.range, to = c(self$r0,self$r1))
                           theta <- theta_rescale_no_clip(y, details$theta.range, arc, dir)
                         }

                         dist_polar(r, theta)
                       },

                       backtransform_range = function(self, panel_params) {
                         self$range(panel_params)
                       },

                       range = function(self, panel_params) {
                         # summarise_layout() expects that the x and y ranges here
                         # match the setting from self$theta and self$r
                         setNames(
                           list(panel_params$theta.range, panel_params$r.range),
                           c(self$theta, self$r)
                         )
                       },

                       setup_panel_params = function(self, scale_x, scale_y, params = list()) {

                         ret <- list(x = list(), y = list())
                         for (n in c("x", "y")) {

                           scale <- get(paste0("scale_", n))
                           limits <- self$limits[[n]]

                           if (self$theta == n) {
                             expansion <- default_expansion(scale, c(0, 0.5), c(0, 0))
                           } else {
                             expansion <- default_expansion(scale, c(0, 0),   c(0, 0))
                           }
                           range <- expand_limits_scale(scale, expansion, coord_limits = limits)

                           out <- scale$break_info(range)
                           ret[[n]]$range <- out$range
                           ret[[n]]$major <- out$major_source
                           ret[[n]]$minor <- out$minor_source
                           ret[[n]]$labels <- out$labels
                           ret[[n]]$sec.range <- out$sec.range
                           ret[[n]]$sec.major <- out$sec.major_source_user
                           ret[[n]]$sec.minor <- out$sec.minor_source_user
                           ret[[n]]$sec.labels <- out$sec.labels
                         }

                         details = list(
                           x.range = ret$x$range, y.range = ret$y$range,
                           x.major = ret$x$major, y.major = ret$y$major,
                           x.minor = ret$x$minor, y.minor = ret$y$minor,
                           x.labels = ret$x$labels, y.labels = ret$y$labels,
                           x.sec.range = ret$x$sec.range, y.sec.range = ret$y$sec.range,
                           x.sec.major = ret$x$sec.major, y.sec.major = ret$y$sec.major,
                           x.sec.minor = ret$x$sec.minor, y.sec.minor = ret$y$sec.minor,
                           x.sec.labels = ret$x$sec.labels, y.sec.labels = ret$y$sec.labels
                         )

                         if (self$theta == "y") {
                           names(details) <- gsub("x\\.", "r.", names(details))
                           names(details) <- gsub("y\\.", "theta.", names(details))
                           details$r.arrange <- scale_x$axis_order()
                         } else {
                           names(details) <- gsub("x\\.", "theta.", names(details))
                           names(details) <- gsub("y\\.", "r.", names(details))
                           details$r.arrange <- scale_y$axis_order()
                         }

                         details
                       },

                       setup_panel_guides = function(self, panel_params, guides, params = list()) {
                         panel_params
                       },

                       train_panel_guides = function(self, panel_params, layers, default_mapping, params = list()) {
                         panel_params
                       },

                       transform = function(self, data, panel_params) {
                         # arc  <- self$start + c(0, self$end)
                         arc <- c(self$start,self$end)
                         dir  <- self$direction
                         data <- rename_data(self, data)

                         donut_new <- rescale(c(self$r0,self$r1),to = c(0,0.4),from = c(0,1))
                         data$r  <- r_rescale(data$r, panel_params$r.range, donut = donut_new)
                         data$theta <- theta_rescale(data$theta, panel_params$theta.range, arc, dir)
                         # data$x <- data$r * sin(data$theta) + 0.5
                         # data$y <- data$r * cos(data$theta) + 0.5

                         data$x <- data$r * cos(data$theta) + 0.5
                         data$y <- data$r * sin(data$theta) + 0.5

                         data
                       },

                       render_axis_v = function(self, panel_params, theme) {
                         arrange <- panel_params$r.arrange %||% c("primary", "secondary")

                         donut_new <- rescale(c(self$r0,self$r1),to = c(0,0.4),from = c(0,1))
                         x <- r_rescale(panel_params$r.major, panel_params$r.range) + 0.5
                         panel_params$r.major <- x
                         if (!is.null(panel_params$r.sec.major)) {
                           panel_params$r.sec.major <- r_rescale(
                             panel_params$r.sec.major,
                             panel_params$r.sec.range
                           ) + 0.5
                         }

                         list(
                           left = render_axis(panel_params, arrange[1], "r", "left", theme),
                           right = render_axis(panel_params, arrange[2], "r", "right", theme)
                         )
                       },

                       render_axis_h = function(panel_params, theme) {
                         list(
                           top = zeroGrob(),
                           bottom = draw_axis(NA, "", "bottom", theme)
                         )
                       },

                       render_bg = function(self, panel_params, theme) {
                         panel_params <- rename_data(self, panel_params)
                         # arc <- self$start + c(0,  self$end)
                         arc <- c(self$start,self$end)
                         dir <- self$direction

                         theta <- if (length(panel_params$theta.major) > 0)
                           theta_rescale(panel_params$theta.major, panel_params$theta.range, arc, dir)
                         thetamin <- if (length(panel_params$theta.minor) > 0)
                           theta_rescale(panel_params$theta.minor, panel_params$theta.range, arc, dir)
                         thetafine <- seq(self$start,  self$end, length.out = 100)

                         donut_new <- rescale(c(self$r0,self$r1),to = c(0,0.4),from = c(0,1))
                         rfine <- c(r_rescale(panel_params$r.major, panel_params$r.range), max(donut_new))

                         # This gets the proper theme element for theta and r grid lines:
                         #   panel.grid.major.x or .y
                         majortheta <- paste("panel.grid.major.", self$theta, sep = "")
                         minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
                         majorr     <- paste("panel.grid.major.", self$r,     sep = "")

                         ggname("grill", grobTree(
                           element_render(theme, "panel.background"),
                           if (length(theta) > 0) element_render(
                             theme, majortheta, name = "angle",
                             x = vec_interleave(0, max(donut_new) * cos(theta)) + 0.5,
                             y = vec_interleave(0, max(donut_new) * sin(theta)) + 0.5,
                             id.lengths = rep(2, length(theta)),
                             default.units = "native"
                           ),
                           if (length(thetamin) > 0) element_render(
                             theme, minortheta, name = "angle",
                             x = vec_interleave(0, max(donut_new) * cos(thetamin)) + 0.5,
                             y = vec_interleave(0, max(donut_new) * sin(thetamin)) + 0.5,
                             id.lengths = rep(2, length(thetamin)),
                             default.units = "native"
                           ),

                           element_render(
                             theme, majorr, name = "radius",
                             x = rep(rfine, each = length(thetafine)) * rep(cos(thetafine), length(rfine)) + 0.5,
                             y = rep(rfine, each = length(thetafine)) * rep(sin(thetafine), length(rfine)) + 0.5,
                             id.lengths = rep(length(thetafine), length(rfine)),
                             default.units = "native"
                           )
                         ))
                       },

                       render_fg = function(self, panel_params, theme) {
                         if (is.null(panel_params$theta.major)) {
                           return(element_render(theme, "panel.border"))
                         }
                         # arc <- self$start + c(0, self$end)
                         arc <- c(self$start,self$end)
                         dir <- self$direction

                         theta <- theta_rescale(panel_params$theta.major, panel_params$theta.range, arc, dir)
                         labels <- panel_params$theta.labels

                         # Combine the two ends of the scale if they are close
                         theta <- theta[!is.na(theta)]
                         ends_apart <- (theta[length(theta)] - theta[1]) %% (2*pi)
                         if (length(theta) > 0 && ends_apart < 0.05 && !is.null(labels)) {
                           n <- length(labels)
                           if (is.expression(labels)) {
                             combined <- substitute(paste(a, "/", b),
                                                    list(a = labels[[1]], b = labels[[n]]))
                           } else {
                             combined <- paste(labels[1], labels[n], sep = "/")
                           }
                           labels[[n]] <- combined
                           labels <- labels[-1]
                           theta <- theta[-1]
                         }

                         donut_new <- rescale(c(self$r0,self$r1),to = c(0,0.4),from = c(0,1))

                         # check text angles
                         ag_raw <- as.theta(theta)
                         if(self$nice.facing == TRUE){
                           lapply(seq_along(ag_raw), function(x){
                             rot <- ag_raw[x]
                             if(rot >= 0 & rot <= 90){
                               new.rot <- rot - 90
                             }else if(rot >= 90 & rot < 180){
                               new.rot <- rot - 90
                             }else if(rot >= 180 & rot < 270){
                               new.rot <- rot - 270
                             }else if(rot >= 270 & rot <= 360){
                               new.rot <- rot - 270
                             }else{
                               new.rot <- rot - 270
                             }

                             res <- list(new.rot,0.5)
                             return(res)
                           }) -> ag_new

                           ag_rot <- sapply(ag_new,"[",1) %>% unlist()
                           ag_just <- sapply(ag_new,"[",2) %>% unlist()
                         }else if(self$x.label.rot == TRUE){
                           lapply(seq_along(ag_raw), function(x){
                             rot <- ag_raw[x]
                             if(rot >= 0 & rot <= 90){
                               new.rot <- rot
                               label.just <- 0
                             }else if(rot >= 90 & rot < 180){
                               new.rot <- rot - 180
                               label.just <- 1
                             }else if(rot >= 180 & rot < 270){
                               new.rot <- rot - 180
                               label.just <- 1
                             }else if(rot >= 270 & rot <= 360){
                               new.rot <- rot - 360
                               label.just <- 0
                             }else{
                               label.just <- 0
                               new.rot <- rot - 360
                             }

                             res <- list(new.rot,label.just)
                             return(res)
                           }) -> ag_new

                           ag_rot <- sapply(ag_new,"[",1) %>% unlist()
                           ag_just <- sapply(ag_new,"[",2) %>% unlist()
                         }else{
                           ag_rot = 0
                           ag_just = 0.5
                         }

                         # check label position
                         if(self$x.label.pos == "top"){
                           label.r <- max(donut_new) + self$x.label.shift
                         }else{
                           label.r <- min(donut_new) - self$x.label.shift
                           if(self$x.label.rot == TRUE) ag_just <- abs(ag_just - 1)
                         }

                         # =============================================================
                         # y axis labels
                         y.label.r <- rescale(x = panel_params$r.major,
                                              from = panel_params$r.range,
                                              to = donut_new)
                         r.labels <- panel_params$r.labels

                         # check position
                         if(self$y.label.pos == "left"){
                           theta.y <-  as.theta(self$end)
                           theta.y.0 <- as.theta(self$end) + self$y.label.shift
                         }else{
                           theta.y <-  as.theta(self$start)
                           theta.y.0 <- as.theta(self$start) - self$y.label.shift
                         }

                         # y label rot
                         if(theta.y >= 0 & theta.y <= 90){
                           new.rot.y <- theta.y - 90
                           y.hjust <- 1
                         }else if(theta.y >= 90 & theta.y < 180){
                           new.rot.y <- theta.y - 90
                           y.hjust <- 1
                         }else if(theta.y >= 180 & theta.y < 270){
                           new.rot.y <- theta.y - 270
                           y.hjust <- 0
                         }else if(theta.y >= 270 & theta.y <= 360){
                           new.rot.y <- theta.y - 270
                           y.hjust <- 0
                         }else{
                           new.rot.y <- theta.y - 270
                           y.hjust <- 0
                         }

                         if(self$y.label.pos == "right"){
                           y.hjust <- abs(y.hjust - 1)
                         }

                         if(self$direction == -1){
                           y.hjust <- abs(y.hjust - 1)
                         }

                         y_label_x <- unit(y.label.r * cos(as.radian(theta.y.0)) + 0.5, "native")
                         y_label_y <- unit(y.label.r * sin(as.radian(theta.y.0)) + 0.5, "native")

                         # =========================================================

                         # label grob
                         grobTree(
                           # build x axis labels
                           if (length(labels) > 0){
                             element_render(theme, "axis.text.x",
                                            labels,
                                            angle = ag_rot,
                                            unit(label.r * cos(theta) + 0.5, "native"),
                                            unit(label.r * sin(theta) + 0.5, "native"),
                                            hjust = ag_just, vjust = 0.5)
                           },
                           # build y axis labels
                           if(self$y.label.show == TRUE){
                             if (length(r.labels) > 0){
                               element_render(theme, "axis.text.y",
                                              r.labels,
                                              angle = new.rot.y,
                                              x = y_label_x,
                                              y = y_label_y,
                                              hjust = y.hjust, vjust = 0.5)

                             }
                           }else{
                             zeroGrob()
                           }
                           ,
                           element_render(theme, "panel.border")
                         )
                       },

                       labels = function(self, labels, panel_params) {
                         if (self$theta == "y") {
                           list(x = labels$y, y = labels$x)
                         } else {
                           labels
                         }
                       },

                       modify_scales = function(self, scales_x, scales_y) {
                         if (self$theta != "y")
                           return()

                         lapply(scales_x, scale_flip_position)
                         lapply(scales_y, scale_flip_position)
                       }
)







rename_data <- function(coord, data) {
  if (coord$theta == "y") {
    rename(data, c("y" = "theta", "x" = "r"))
  } else {
    rename(data, c("y" = "r", "x" = "theta"))
  }
}

theta_rescale_no_clip <- function(x, range, arc = c(0, 2 * pi), direction = 1) {
  rescale(x, to = arc, from = range) * direction
}

theta_rescale <- function(x, range, arc = c(0, 2 * pi), direction = 1) {
  x <- squish_infinite(x, range)
  rescale(x, to = arc, from = range) %% (2 * pi) * direction
}

r_rescale <- function(x, range, donut = c(0, 0.4)) {
  x <- squish_infinite(x, range)
  rescale(x, donut, range)
}
