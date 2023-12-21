#' Create custom facet wrap for features
#'
#' This function creates a custom facet wrap for features in ggplot2.
#'
#' @param facets A character vector specifying the facet variable(s).
#' @param ncol The number of columns in the facet grid (default is 4).
#' @param scales Scales argument for facet_wrap (default is "free").
#' @param strip.col Background color for facet strip labels (optional).
#' @param ... Additional arguments passed to ggh4x::facet_wrap2.
#'
#' @return A custom facet wrap for feature-based plotting.
#' @export
facet_feature <- function(facets = "gene_name",ncol = 4,scales = "free",
                          strip.col = NULL,...){
  list(ggh4x::facet_wrap2(facets = facets,ncol = ncol,scales = scales,
                          strip = ggh4x::strip_themed(background_x =
                                                        ggh4x::elem_list_rect(fill = strip.col),
                          ),...),
       theme_bw())
}




#' Create custom facet grid for hetamap plotting
#'
#' This function creates a custom facet grid for hetamap plotting in ggplot2.
#'
#' @param facet_col A character vector specifying the columns for facetting (optional).
#' @param facet_row A character vector specifying the rows for facetting (optional).
#' @param scales Scales argument for facet_grid2 (default is "free_x").
#' @param strip.col Background color for facet strip labels (optional).
#' @param space Space argument for facet_grid2 (default is "free_x").
#' @param no.xlabel Whether remove x axis labels (default is "TRUE").
#' @param ... Additional arguments passed to ggh4x::facet_grid2.
#'
#' @return A custom facet grid for hetamap plotting.
#' @export
facet_hetamap <- function(facet_col = NULL,
                          facet_row = NULL,
                          scales = "free_x",
                          strip.col = NULL,
                          space = "free_x",
                          no.xlabel = TRUE,
                          ...){
  if(!is.null(facet_col)){
    facet_col <- vars(!!!rlang::ensyms(facet_col))
  }

  if(!is.null(facet_row)){
    facet_row <- vars(!!!rlang::ensyms(facet_row))
  }

  if(no.xlabel == TRUE){
    ele.tick <- element_blank()
    ele.text <- element_blank()
  }else{
    ele.tick <- element_line()
    ele.text <- element_text()
  }

  list(ggh4x::facet_grid2(rows = facet_row,cols = facet_col,
                          scales = scales,
                          strip = ggh4x::strip_themed(background_x =
                                                        ggh4x::elem_list_rect(fill = strip.col),
                                                      background_y =
                                                        ggh4x::elem_list_rect(fill = strip.col),
                          ),
                          space = space,
                          ...),
       theme_bw() +
         theme(axis.ticks.x = ele.tick,
               axis.text.x = ele.text,
               strip.text = element_text(face = "bold.italic"),
               strip.placement = "outside",
               panel.grid = element_blank()))
}



#' Custom ggplot2 facet function: facet_new
#'
#' This custom ggplot2 facet function, \code{facet_new}, allows you to create
#' customized facet grids in ggplot2, with additional options for appearance and scales.
#'
#' @param facet_col Columns to facet by (can be multiple columns).
#' @param facet_row Rows to facet by (can be multiple rows).
#' @param scales The type of scaling to use for facets (default is "free_x").
#' @param strip.col Background color for facet strip labels.
#' @param space The type of spacing to use for facets (default is "free_x").
#' @param no.xlabel Logical value indicating whether to hide x-axis labels (default is FALSE).
#' @param no.ylabel Logical value indicating whether to hide y-axis labels (default is FALSE).
#' @param title.hjust Horizontal justification for facet titles (0 for left, 0.5 for center, 1 for right).
#' @param t Top margin size for the plot (in npc units).
#' @param r Right margin size for the plot (in npc units).
#' @param b Bottom margin size for the plot (in npc units).
#' @param l Left margin size for the plot (in npc units).
#' @param low.col Color for the low end of the gradient.
#' @param high.col Color for the high end of the gradient.
#' @param x.angle Angle for x-axis labels (default is 0).
#' @param x.label.hjust X axis label hjust (default is 0.5).
#' @param legend.position Position of the legend (e.g., "top", "bottom", "right", "none").
#' @param ... Additional arguments to be passed to \code{\link[ggh4x]{facet_grid2}}.
#'
#' @return A list of ggplot2 elements, including facet_grid settings, theme settings,
#'
#' @importFrom ggh4x facet_grid2 strip_themed elem_list_rect
#'
#' @export
facet_new <- function(facet_col = NULL,
                      facet_row = NULL,
                      scales = "free_x",
                      strip.col = NULL,
                      space = "free_x",
                      no.xlabel = FALSE,
                      no.ylabel = FALSE,
                      title.hjust = 0,
                      t = 0.025,r = 0.1,b = 0.025,l = 0.1,
                      low.col = "grey90",high.col = "red",
                      x.angle = 0,x.label.hjust = 0.5,
                      legend.position = NULL,
                      ...){
  if(!is.null(facet_col)){
    facet_col <- vars(!!!rlang::ensyms(facet_col))
  }

  if(!is.null(facet_row)){
    facet_row <- vars(!!!rlang::ensyms(facet_row))
  }

  if(no.xlabel == TRUE){
    ele.tick.x <- element_blank()
    ele.text.x <- element_blank()
  }else{
    ele.tick.x <- element_line()
    ele.text.x <- element_text(angle = x.angle,hjust = ifelse(x.angle == 45,1,x.label.hjust))
  }

  if(no.ylabel == TRUE){
    ele.tick.y <- element_blank()
    ele.text.y <- element_blank()
  }else{
    ele.tick.y <- element_line()
    ele.text.y <- element_text()
  }

  list(ggh4x::facet_grid2(rows = facet_row,cols = facet_col,
                          scales = scales,
                          strip = ggh4x::strip_themed(background_x =
                                                        ggh4x::elem_list_rect(fill = strip.col),
                                                      background_y =
                                                        ggh4x::elem_list_rect(fill = strip.col),
                          ),
                          space = space,
                          ...),
       theme_bw() +
         theme(axis.ticks.x = ele.tick.x,
               axis.text.x = ele.text.x,
               axis.ticks.y = ele.tick.y,
               axis.text.y = ele.text.y,
               axis.text = element_text(colour = "black"),
               strip.text = element_text(face = "bold.italic"),
               strip.placement = "outside",
               legend.position = legend.position,
               legend.background = element_blank(),
               plot.margin = margin(t = t,r = r,b = b,l = l,unit = "npc"),
               panel.grid = element_blank()),
       scale_fill_gradient(low = low.col,high = high.col),
       guides(size = guide_legend(title = "Fraction of cells\nin group (%)",
                                  title.hjust = title.hjust),
              fill = guide_colorbar(title = "Mean expression\n in group",
                                    title.hjust = title.hjust,
                                    frame.colour = "black",
                                    ticks.colour = "black"))
  )
}
