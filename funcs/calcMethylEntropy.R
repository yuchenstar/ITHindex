

#' Title
#' calculate Shannon entropy given the beta-values for a sample
#' The formula used is the same as the one reported by Hannum et al. 2013
#'
#' @param data  numeric character containing the beta-values for a sample
#' @param N the Number of the methylation markers. The defaults value is the length of data
#'
#' @return entropy a numeric vector of length 1
#' @export
#'
#' @examples
calcMethylEntropy <- function(data,N = length(data)) {

  data <- as.numeric(data)
  data <- data[!is.na(data)]

  if (length(data) == 0) return(0)

  N = length(data)

  data[data==0] <- 0.000000000000001;
  data[data==1] <- 0.999999999999999;

  # MFi is the methylation fraction of the i-th methylation marker
  entropy <- (1 / (N * log(1/2))) * sum(
    sapply(data, function(mfi){
      (mfi * log2(mfi)) + ((1 - mfi) * log2(1 - mfi))
    })
  )

  return(entropy)
}

ldy_calcMethylEntropy <- function(data){

  data_cols <- c("gene_id")
  if (!is.data.frame(data) | length(grep(FALSE, data_cols %in% colnames(data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  expr <- data %>%
    tibble::column_to_rownames(var = "gene_id")

  expr <- naniar::impute_mean_all(expr)

  out <- data.frame(SampleID = colnames(expr),
                    MethyEntropy = round(as.numeric(apply(expr,2,calcMethylEntropy)),4)) %>%
    dplyr::select(SampleID,`Shannon entropy` = MethyEntropy)
  return(out)
}
