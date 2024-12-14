
#' @title  Infer the ITH value by MAD algorithm
#' @description Infer the ITH value by MAD algorithm based on CCF data or mutation data
#'
#' @param mutation_data signleregional mutation data，mandatory fields: Tumor_VAF or CCF (if cancer_cell_fraction is TRUE )
#' @param cancer_cell_fraction FALSE. Is data have cancer_cell_fraction (CCF) fields.
#'
#' @return math value
#' @export
#'
#' @examples
calcMATH <- function(mutation_data,cancer_cell_fraction = FALSE){
  # 针对单个样本进行ITH计算

  if(cancer_cell_fraction){
    data_cols <- c("CCF")
    if (!is.data.frame(mutation_data) | length(grep(FALSE, data_cols %in% colnames(mutation_data))) != 0) {
      print(paste("Error, cancer_cell_fraction is TRUE, but the following columns: ",
                  paste(data_cols, collapse=" ")," is missing!"))

      # message("Error, cancer_cell_fraction is TRUE, but the following columns: ",
      #         paste(data_cols, collapse=" ")," is missing!")
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
    mutation_data$Tumor_VAF <- mutation_data$CCF
  }else{
    data_cols <- c("Tumor_VAF")
    if (!is.data.frame(mutation_data) | length(grep(FALSE, data_cols %in% colnames(mutation_data))) != 0) {
      print(paste("Error, data must be a dataframe, and contain the following columns: ",
                  paste(data_cols, collapse=" ")))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
  }

  mutation_data <- mutation_data %>%
    dplyr::filter(!is.na(Tumor_VAF))
  vaf <- as.numeric(mutation_data$Tumor_VAF)

  median_vaf <- median(vaf)
  mad <- mad(vaf) # median(abs(vaf - median_vaf)) * 1.4826
  math <- 100*mad/median_vaf

  # abs.med.dev = abs(vaf - median(vaf)) # absolute deviation from median vaf
  # pat.mad = median(abs.med.dev) * 100
  # math = pat.mad * 1.4826 /median(vaf)

  return(math)
}

#' Title
#'
#' @param mutation_data signleregional mutation data，mandatory fields: SampleID, Tumor_VAF or CCF (if cancer_cell_fraction is TRUE )
#' @param cancer_cell_fraction FALSE. Is data have cancer_cell_fraction (CCF) fields.
#'
#' @return
#' @export
#'
#' @examples
lpy_calcMATH <- function(mutation_data,
                         cancer_cell_fraction = FALSE,
                         show_progress = TRUE){

  if(cancer_cell_fraction){
    data_cols <- c("SampleID", "CCF")
    if (!is.data.frame(mutation_data) | length(grep(FALSE, data_cols %in% colnames(mutation_data))) != 0) {
      print(paste("Error, cancer_cell_fraction is TRUE, but the following columns: ",
                  paste(data_cols, collapse=" ")," is missing!"))

      # message("Error, cancer_cell_fraction is TRUE, but the following columns: ",
      #         paste(data_cols, collapse=" ")," is missing!")
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
    mutation_data$Tumor_VAF <- mutation_data$CCF
  }else{
    data_cols <- c("SampleID", "Tumor_VAF")
    if (!is.data.frame(mutation_data) | length(grep(FALSE, data_cols %in% colnames(mutation_data))) != 0) {
      print(paste("Error, data must be a dataframe, and contain the following columns: ",
                  paste(data_cols, collapse=" ")))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
  }


  sids <- unique(mutation_data$SampleID)
  n <- length(sids)
  out <- do.call(rbind,lapply(seq_len(n),function(i){

    sid  <- sids[i]

    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate MATH, Sample ID:", sid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    p_math <- mutation_data %>%
      dplyr::filter(SampleID == sid) %>%
      calcMATH(cancer_cell_fraction = cancer_cell_fraction)

    p_index <- data.frame("SampleID" = sid,
                          "MATH" = round(p_math,4))

    return(p_index)
  }))
  return(out)
}
