
#' @title Infer the ITH value based on the ration of CCF
#' @description
#' Infer the ITH value based on the ratio of clonal in tumors.
#'
#' @param ccf_data signleregional mutation data，mandatory fields: SampleID, CCF
#' @param clonal_ratio_ccf cutoff of ratio. Default=0.8. >= cutoff was defined as clonal, otherwise subclonal.
#'
#' @return the ratio of clonal
#' @export
#'
#' @examples
calcPClonality <- function(ccf_data,
                            clonal_ratio_ccf = 0.8){
  # 针对单个样本进行ITH计算
  data_cols <- c("CCF")
  if (!is.data.frame(ccf_data) | length(grep(FALSE, data_cols %in% colnames(ccf_data))) != 0) {
    print(paste("Error, cancer_cell_fraction is TRUE, but the following columns: ",
                paste(data_cols, collapse=" ")," is missing!"))

    # message("Error, cancer_cell_fraction is TRUE, but the following columns: ",
    #         paste(data_cols, collapse=" ")," is missing!")
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }

  number_data <- ccf_data %>%
    dplyr::mutate(clonal = ifelse(CCF >= clonal_ratio_ccf,1,0))
  # The ratio of Clonal is defined as a ITH index？
  ratio <- nrow(number_data[which(number_data$clonal == 1),])/nrow(number_data)
  return(round(ratio,4))

}


#' Title
#'
#' @param ccf_data signleregional mutation data，mandatory fields: SampleID, CCF
#' @param clonal_ratio_ccf cutoff of ratio. Default=0.8. >= cutoff was defined as clonal, otherwise subclonal.
#'
#' @return
#' @export
#'
#' @examples
lpy_PClonality <- function(ccf_data,
                                clonal_ratio_ccf = 0.8,
                                show_progress = TRUE){

  data_cols <- c("SampleID", "CCF")
  if (!is.data.frame(ccf_data) | length(grep(FALSE, data_cols %in% colnames(ccf_data))) != 0) {
    print(paste("Error, cancer_cell_fraction is TRUE, but the following columns: ",
                paste(data_cols, collapse=" ")," are missing!"))

    # message("Error, cancer_cell_fraction is TRUE, but the following columns: ",
    #         paste(data_cols, collapse=" ")," is missing!")
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  sids <- unique(ccf_data$SampleID)
  n = length(sids)
  out <- do.call(rbind,lapply(seq_len(length(sids)),function(i){

    sid <- sids[i]
    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate pClonality, Sample ID:", sid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    p_ratio <- ccf_data %>%
      dplyr::filter(SampleID == sid) %>%
      calcPClonality(clonal_ratio_ccf = clonal_ratio_ccf)

    p_index <- data.frame("SampleID" = sid,
                          "pClonality" = p_ratio)

    return(p_index)
  }))
  return(out)
}
