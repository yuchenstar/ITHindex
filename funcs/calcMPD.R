#' Title Quantification of intra-tumor heterogeneity via distance and PCA
#'
#' @param data a numeric matrix, data frame with row name is feature, column is sample.
#' @param pca logical value indicating whether execute PCA analysis
#' @param nPCs if PCA is TRUE, the number of PC used to distance calculate.
#' @param method  the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @return
#' @export
#'
#' @examples
calcMPD <- function(data, pca=F, nPCs=2, method = 'euclidean'){

  if (pca) {
    fit = prcomp(t(data), center=T, scale.=F)
    m_dst = dist(fit$x[,1:min(nPCs, ncol(fit$x))],method = method)
  }else{
    m_dst = dist(t(data),method = method)
  }
  ## quantify ITH
  return(mean(c(m_dst)))
}


#' Title
#'
#' @param data a numeric matrix, data frame with row name is feature, column is sample.
#' @param pca logical value indicating whether execute PCA analysis
#' @param nPCs if PCA is TRUE, the number of PC used to distance calculate.
#' @param method  the distance measure to be used. This must be one of "euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski".
#'
#' @return
#' @export
#'
#' @examples
lpy_MPD <- function(data,meta,pca=F, nPCs=2, method = 'euclidean',show_progress = TRUE){

  data_cols <- c("PatientID", "SampleID")
  if (!is.data.frame(meta) | length(grep(FALSE, data_cols %in% colnames(meta))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  data_cols <- c("gene_id")
  if (!is.data.frame(data) | length(grep(FALSE, data_cols %in% colnames(data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  phen <- meta
  pids <- unique(phen$PatientID)

  expr <- data %>%
    tibble::column_to_rownames(var = "gene_id")
    # dplyr::select(-PatientID) %>%
    # t() %>%
    # as.data.frame()

  n <- length(pids)
  out <- do.call(rbind, lapply(seq_len(n), function(i) {
    # x = pid[1]

    pid <- pids[i]

    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate sMPD, Patient ID:", pid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    s_pheno <- phen[phen$PatientID == pid, ]
    s_expr <- expr[, s_pheno$SampleID]
    s_index <- calcMPD(data = s_expr,pca = pca,nPCs = nPCs,method = method)
    s_out <- data.frame(
      PatientID = pid,
      MPD = round(s_index,4)
    )
  }))

  return(out)
}
