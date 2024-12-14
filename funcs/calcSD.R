#' Title
#' SD-Yielding Intra-Tumor Heterogeneity.
#'
#' This function calculates the ITH index in a bulk tumor based on the variations of gene exprerssion or methylation levels.
#' @param data A dataframe or matrix containing gene expression (Normalized) or methylation (beta-values)  profiles in tumors. The row name is gene symbol and the column name is tumor sample ID.
#' @param adjusted Logical value. If TRUE, profiles were adjusted by row means.
#'
#' @return A dataframe with 2 columns: SampleID and SD
#' @export
#'
#' @examples
calcSD <- function(data, adjusted = TRUE, imputed = TRUE){

  data_cols <- c('gene_id')
  if (!is.data.frame(data) | length(grep(FALSE, data_cols %in% colnames(data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  data %<>%
    tibble::column_to_rownames(var = 'gene_id')

  if (imputed) data <- naniar::impute_mean_all(data)
  if (adjusted) data <- (data - rowMeans(data,na.rm = T))^2

  sd <- apply(data, 2, sd, na.rm = TRUE)
  sd_index <- data.frame(SampleID = names(sd),
                         mSD = round(sd,4),
                         row.names = NULL)
  return(sd_index)

}
