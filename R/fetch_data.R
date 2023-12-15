globalVariables(c("value",".","gene_name","Dim1","Dim2"))

#' Fetch data from Seurat object
#'
#' This function retrieves and combines single-cell data, including reduction data,
#' metadata, and gene expression data from a Seurat object.
#'
#' @param object A Seurat object containing single-cell data.
#' @param reduction The reduction method to use (default is "umap").
#' @param features A vector of features (genes) to fetch expression data for (optional).
#' @param featuresAnno Annotation level for features (default is 0).
#' @param pct.exp.var A variable for percentage of expressed variance (default is "seurat_clusters").
#' @param slot The data slot to use from the Seurat object ("data" or "counts").
#'
#' @return A data frame containing the combined single-cell data.
#' @export
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' sc_data <- fetch_data(object = my_seurat_object, reduction = "umap")
#' head(sc_data)
#' }
#'
#' @family Single-cell data functions
#'
#' @importFrom Seurat Embeddings FetchData PercentAbove
#' @import dplyr
#' @import reshape2
#' @import lifecycle
#'
#' @rdname fetch_data
fetch_data <- function(object = NULL,
                       reduction = "umap",
                       features = NULL,
                       featuresAnno = 0,
                       pct.exp.var = "seurat_clusters",
                       slot = c("data","counts")){
  slot <- match.arg(slot,c("data","counts"))

  # get reduction data
  reduc <- data.frame(Seurat::Embeddings(object, reduction = reduction))
  colnames(reduc) <- paste0("Dim",1:2)

  # metadata
  meta <- object@meta.data
  ident <- data.frame(ident = Seurat::Idents(object))
  meta <- cbind(ident,meta)

  # combine
  pc12 <- cbind(reduc, meta)

  # add cell
  pc12$cell <- rownames(pc12)

  # get gene expression
  if(!is.null(features)){
    geneExp <- Seurat::FetchData(object = object, vars = features,slot = slot)

    # cbind
    mer <- cbind(pc12, geneExp)

    # merge data
    megredf <- reshape2::melt(
      mer,
      id.vars = colnames(pc12),
      variable.name = "gene_name",
      value.name = "value"
    )

    # calculate mean expression and median expression
    ave_exp <- megredf %>% dplyr::group_by(.data[["gene_name"]],.data[[pct.exp.var]]) %>%
      dplyr::summarise(mean_exp = mean(value),median_exp = median(value))

    # add feature anno
    # x = 1
    lapply(seq_along(features), function(x){
      tmp <- subset(megredf,gene_name %in% features[x])
      tmp$featureAnno <- featuresAnno[x]

      # add percent expression
      pctexp <- tmp %>% dplyr::group_by(.data[[pct.exp.var]]) %>%
        dplyr::summarise(pct = Seurat::PercentAbove(value,threshold = 0)*100)

      tmp <- tmp %>% dplyr::left_join(y = pctexp,by = pct.exp.var)

      # add mean median exp
      tmp_ave_exp <- subset(ave_exp,gene_name %in% features[x],select = -gene_name)
      tmp <- tmp %>% dplyr::left_join(y = tmp_ave_exp,by = pct.exp.var)

      return(tmp)
    }) %>% Reduce("rbind",.) -> megredf

    megredf <- megredf %>% dplyr::group_by(.data[["gene_name"]]) %>%
      dplyr::arrange(.data[["value"]])

  }else{
    megredf <- pc12
  }

  return(megredf)
}
