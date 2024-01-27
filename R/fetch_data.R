globalVariables(c("value",".","gene_name","Dim1","Dim2","featureAnno","celltype","x","y"))

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
#' @param group.vars The column names in metadata which you want to combine,
#' useful for facet plot, the new combined column name is "group.vars" and value
#' column name is "group.value" (default is NULL).
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
                       group.vars = NULL,
                       slot = c("data","counts")){
  slot <- match.arg(slot,c("data","counts"))

  options(warn=-1)
  # Suppress summarise info
  options(dplyr.summarise.inform = FALSE)

  # ============================================================================
  # get reduction data
  # ============================================================================
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

  # ============================================================================
  # group some variables in meta
  if(!is.null(group.vars)){
    pc12 <- reshape2::melt(pc12,
                           id.vars = setdiff(x = colnames(pc12),y = group.vars),
                           variable.name = "group.vars",
                           value.name = "group.value")
  }

  # ============================================================================

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
      dplyr::summarise(mean_exp = mean(value),median_exp = median(value)) %>%
      dplyr::ungroup()

    # calculate percent expression
    gene_pct <- megredf %>% dplyr::group_by(.data[["gene_name"]],.data[[pct.exp.var]]) %>%
      dplyr::summarise(pct = Seurat::PercentAbove(value,threshold = 0)*100) %>%
      dplyr::ungroup()

    # add feature anno
    fanno <- data.frame(gene_name = features,featureAnno = featuresAnno)

    # merge additional meta
    megredf <- megredf %>%
      dplyr::left_join(y = gene_pct,by = c("gene_name",pct.exp.var)) %>%
      dplyr::left_join(y = ave_exp,by = c("gene_name",pct.exp.var)) %>%
      dplyr::left_join(y = fanno,by = c("gene_name"),
                       relationship = "many-to-many")

    # add feature anno
    # x = 1
    # lapply(seq_along(features), function(x){
    #   tmp <- subset(megredf,gene_name %in% features[x])
    #   tmp$featureAnno <- featuresAnno[x]
    #
    #   # add percent expression
    #   pctexp <- tmp %>% dplyr::group_by(.data[[pct.exp.var]]) %>%
    #     dplyr::summarise(pct = Seurat::PercentAbove(value,threshold = 0)*100)
    #
    #   tmp <- tmp %>% dplyr::left_join(y = pctexp,by = pct.exp.var)
    #
    #   # add mean median exp
    #   tmp_ave_exp <- subset(ave_exp,gene_name %in% features[x],select = -gene_name)
    #   tmp <- tmp %>% dplyr::left_join(y = tmp_ave_exp,by = pct.exp.var)
    #
    #   return(tmp)
    # }) %>% Reduce("rbind",.) -> megredf

    # ==========================================================================
    # add suffix for duplicate features
    gs <- table(features) > 1
    dup_genes <- names(gs)[gs]

    if(length(dup_genes) > 0){
      uni_f <- dup_genes
      lapply(seq_along(uni_f), function(x){
        tmp <- subset(megredf,gene_name %in% uni_f[x])
        anno_f <- unique(tmp$featureAnno)

        # check anno types length
        if(length(anno_f) > 1){
          lapply(seq_along(anno_f), function(x){
            tmp2 <- subset(tmp,featureAnno %in% anno_f[x])
            tmp2$gene_name <- paste0(tmp2$gene_name,"_",x)

            return(tmp2)
          }) %>% Reduce("rbind",.) -> add_name
          return(add_name)
        }else{
          return(tmp)
        }
      }) %>% Reduce("rbind",.) %>% dplyr::arrange(.data[["featureAnno"]]) -> dup_df

      # rbind
      megredf <- rbind(dup_df,subset(megredf,gene_name %in% setdiff(features,dup_genes)))
    }

    # order
    od <- unique(megredf[,c("featureAnno","gene_name")])
    megredf$gene_name <- factor(megredf$gene_name,levels = od$gene_name)

    megredf <- megredf %>% dplyr::group_by(.data[["gene_name"]]) %>%
      dplyr::arrange(.data[["value"]])

  }else{
    megredf <- pc12
  }

  return(megredf)
}
