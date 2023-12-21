#' Calculate Density Estimation
#'
#' This function calculates the kernel density estimation of a given dataset and
#' returns a data frame with density values.
#'
#' @param data A data frame containing the dataset for density estimation.
#' @param trim Logical value indicating whether to trim values outside the data range.
#' @param bw Bandwidth for density estimation (default is "nrd0").
#' @param adjust A bandwidth adjustment factor (default is 1).
#' @param kernel The kernel function to use (default is "gaussian").
#' @param n Number of grid points for density estimation (default is 512).
#'
#' @return A data frame containing density estimation results.
#'
#' @importFrom stats density
#' @importFrom dplyr case_when
calc_density <- function(data = NULL,
                         trim = TRUE,
                         bw = "nrd0",
                         adjust = 1,
                         kernel = "gaussian",
                         n = 512){
  # calculate density
  density_data <- stats::density(data$exp,
                                 bw = bw, adjust = adjust,
                                 kernel = kernel,
                                 n = n)

  # data range
  range_data <- range(data$exp)

  # whether tirm head and tail
  if(trim == TRUE){
    # data frame
    new_daframe <- data.frame(vio_y = density_data$x,vio_x = density_data$y)

    # trim head and tail
    new_daframe$vio_y <- dplyr::case_when(
      new_daframe$vio_y < range_data[1] ~ range_data[1],
      new_daframe$vio_y > range_data[2] ~ range_data[2],
      TRUE ~ new_daframe$vio_y)

  }else{
    # data frame
    new_daframe <- data.frame(vio_y = density_data$x,vio_x = density_data$y)
  }

  # add x y
  new_daframe$x <- data$x[1]
  new_daframe$y <- data$y[1]

  # add median exp
  new_daframe$meadian_exp <- median(data$exp)
  new_daframe
}
