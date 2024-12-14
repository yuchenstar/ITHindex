

#' @Title Infer the ITH by Jaccard Similarity
#' @description Infer the ITH metric by Jaccard algorithm based on multi-regional mutation data
#'
#' @param seta a vector contain the gene symbol with mutation in regional A from Patient 1
#' @param setb a vector contain the gene symbol with mutation in regional B from Patient 1
#'
#' @return Jaccard Similarity
#' @export
#'
#' @examples
calcJaccardSimilarity <- function(seta, setb) {
  intersection = length(intersect(seta, setb))
  union = length(seta) + length(setb) - intersection
  return (intersection/union)
}


#' Title
#'
#' @param mutation_data multiregional mutation data，mandatory fields: PatientID, SampleID, Hugo_Symbol.
#'
#' @return a dataframe with ITH index
#' @export
#'
#' @examples
lpy_JaccardSimilarity <- function(mutation_data){


  data_cols <- c("PatientID", "SampleID", "Hugo_Symbol")
  if (!is.data.frame(mutation_data) | length(grep(FALSE, data_cols %in% colnames(mutation_data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))
    return(NULL)
  }

  pids <- unique(mutation_data$PatientID)
  n <- length(pids)
  out <- do.call(rbind,lapply(seq_len(length(pids)),function(i){

    pid = pids[i]

    update_modal_progress(
      value = i / n,
      text = paste("Calculate Jaccard Similarity, Patient ID:", pid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
    )

    p_data <- mutation_data %>%
      dplyr::filter(PatientID %in% pid)
    s_list <- split(p_data$Hugo_Symbol,p_data$SampleID)
    if(length(s_list) > 1){
      p_index <- c()
      for (i in seq_len(length(s_list)-1)) {
        for (j in seq(i+1,length(s_list))) {
          index = calcJaccardSimilarity(s_list[[i]],s_list[[j]])
          p_index <- c(p_index,index)
        }
      }
      # 计算算数平均数
      p_index_m <- mean(p_index,na.rm = T) %>% round(4)
    }else{
      p_index_m = NA_real_
    }
    j_index <- data.frame("PatientID" = pid,
                          "JaccardSimilarity" = p_index_m)
    return(j_index)
  }))

  out %<>%
    dplyr::select(PatientID,`Jaccard Similarity` = JaccardSimilarity)
  return(out)
}
