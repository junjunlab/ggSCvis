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
               panel.grid = element_blank()))
}
