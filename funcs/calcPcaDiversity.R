

#' Title Defined a diversity score to measure the degree of intra-tumoral heterogeneity.
#' It was calculated based on the gene expression profiles of malignant cells within the tumor.
#' PCA was employed to project the original expression profiles of all malignant cells to the eigenvector space to derive PCs, which could capture major information and reduce noise.
#'
#' @param data a dataframe or matrix containing gene expression (Normalized, e.g: TPM), The first row name is gene symbol and the others are  cell sample ID. The mandatory fields: hugo_symbol
#' @param phen a data frame containing the meta information. The mandatory fields: PatientID, SampleID.
#' @param topPC logical value indicating whether select top PCs for analysis
#' @param nPCs  if PCA is TRUE, the number of PC used to distance calculate.
#' @param rmOutlier  logical value, removed the extreme values that were beyond the range of x standard deviations from the mean
#' @param sdTimes
#' @param sampleDropPercent  A cell will be drop if more than xx perent PC were outliers. xx is sampleDropPercent
#'
#' @return
#' @export
#'
#' @examples
calcPcaDiversity <- function(data,
                             phen,
                             topPC = TRUE,
                             nPCs = 30,
                             rmOutlier = TRUE,
                             sdTimes = 3,
                             sampleDropPercent = 0.5,
                             coordinate = NULL,
                             show_progress = TRUE){



  data_cols <- c("PatientID", "SampleID")
  if (!is.data.frame(phen) | length(grep(FALSE, data_cols %in% colnames(phen))) != 0) {
    print(paste("Error, phen must be a dataframe, and contain the two following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  phen_stat <- phen %>%
    dplyr::group_by(PatientID) %>%
    dplyr::summarise(Cell_Num = dplyr::n())

  # 每个患者/样本至少要有2个以上cell
  phen_with_m2cells <- phen_stat %>%
    dplyr::filter(Cell_Num > 1)

  if(nrow(phen_with_m2cells) < 1){
    print(paste("Error, each PatientID must has at least two or more cellular samples (CellIDs))!"))

    # message("Error, each PatientID must has at least two or more cellular samples (CellIDs))!")
    # stop(message(paste0("Error, each PatientID must has at least two or more cellular samples (CellIDs))!")))

    return(NULL)
  }

  data_cols <- c("hugo_symbol")
  if (!is.data.frame(data) | length(grep(FALSE, data_cols %in% colnames(data))) != 0|ncol(data) < 3) {
    print(paste("Error, data must be a dataframe, and must contain the following columns: ",
                paste(data_cols, collapse=""),", and sample names."))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  data <- data %>%
    tibble::column_to_rownames(var = 'hugo_symbol')

  if(length(intersect(colnames(data),phen$SampleID)) == 0){
    print(paste("Error, no match cell ids were found between the column names of data and phen's SampleID!"))

    # message("Error, no match cell ids were found!")
    # stop(message(paste0("Error, no match cell ids were found!")))

    return(NULL)
  }

  fit <- prcomp(t(data), center=T, scale.=F)
  pc_data <- fit$x[,1:ncol(fit$x)]

  pids <- unique(phen_with_m2cells$PatientID)
  n = length(pids)
  out <- do.call(rbind,lapply(seq_len(n),function(i){

    pid = pids[i]

    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate Diversity, Patient ID:", pid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    message("Patient ID: ",pid)
    sid <- phen[which(phen$PatientID == pid),]$SampleID
    temp_pca <- pc_data[sid,]
    # select top number pc
    if(topPC){
      if(nPCs >= ncol(temp_pca))  nPCs = ncol(temp_pca)
      temp_pca <- temp_pca[,1:nPCs]
    }
    # removed the extreme values that were beyond the range of x standard deviations from the mean
    if(rmOutlier){
      cutoff1 <- apply(temp_pca, 2, function(x) mean(x) - sdTimes*sd(x))
      cutoff2 <- apply(temp_pca, 2, function(x) mean(x) + sdTimes*sd(x))
      # A cell will be drop if more than xx perent PC were outliers. xx is sampleDropPercent
      cell_bool <- apply(temp_pca, 1, function(x) sum(x < cutoff1) < (sampleDropPercent * ncol(temp_pca)) & sum(x > cutoff2) < (sampleDropPercent * ncol(temp_pca)))
      temp_pca <- temp_pca[cell_bool,]
    }

    if(nrow(temp_pca) < 1){
      diversity_value = NA
    }else{
      mean_pca <- apply(temp_pca, 2, function(y) mean(y))
      diversity_value <- mean(apply(temp_pca, 1, function(x) sqrt(sum((x-mean_pca)^2))))
    }

    diversity_df <- data.frame("PatientID" = pid,
                               "Diversity_score" = round(diversity_value,4))

  }))

  if(!is.null(coordinate)){
    message("Calculate the physical diversity score for each patient use the spatial coordinates of each sample")
    coord <- coordinate %>%
      calcPhysicalDiversity()
    out <- dplyr::left_join(out,coord,by='PatientID') %>%
      dplyr::mutate(normalizedDiv = round(Diversity_score/PhysicalDiversity,4)) %>%
      dplyr::select(PatientID,
                    `Diversity score` = normalizedDiv)
  }else{
    # without physical diversity score
    out %<>%
      dplyr::select(PatientID,
                    `Diversity score` = Diversity_score)
  }

  return(out)

}


#' Title
#'
#' @param data Spatial coordinates of each sample. The mandatory fields: PatientID, SampleID, x, y.
#'
#' @return a dataframe with physical diversity score of each patient.
#' @export
#'
#' @examples
calcPhysicalDiversity <- function(data){

  data_cols <- c('PatientID','SampleID',"x", "y")
  if (!is.data.frame(data) | length(grep(FALSE, data_cols %in% colnames(data))) != 0| ncol(data) !=4) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  data$x <- as.numeric(data$x)
  data$y <- as.numeric(data$y)

  pids <- unique(data$PatientID)

  out <- do.call(rbind,lapply(pids,function(pid){
    p_data <- data %>%
      dplyr::filter(PatientID == pid)
    mean_x <- mean(p_data$x,na.rm = T)
    mean_y <- mean(p_data$y,na.rm = T)

    div_phy <- mean(apply(p_data, 1, function(rr) sqrt(sum((as.numeric(rr[3])- mean_x)^2,(as.numeric(rr[4])- mean_y)^2))))

    # test
    # sqrt((3.77018-mean_x)^2 + (7.91001-mean_y)^2 ) +
    # sqrt((3.32201-mean_x)^2 + (4.60350-mean_y)^2 ) +
    # sqrt((6.06015-mean_x)^2 + (7.28393-mean_y)^2 ) +
    # sqrt((6.58566-mean_x)^2 + (4.49064-mean_y)^2 ) +
    # sqrt((4.86342-mean_x)^2 + (5.90644-mean_y)^2 ) +
    # sqrt((2.88065-mean_x)^2 + (6.55242-mean_y)^2 )

    p_out <- data.frame(
      PatientID = pid,
      PhysicalDiversity = round(div_phy,4)
    )
    return(p_out)
  }))

}
