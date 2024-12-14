

#' @title  Infer the ITH value by Shannon index
#' @description Infer the ITH value by Shannon index based on CCF data or VAF data
#'
#' @param mutation_data signleregional mutation data，mandatory fields: Tumor_VAF or CCF (if cancer_cell_fraction is TRUE )
#' @param cancer_cell_fraction FALSE. Is data have cancer_cell_fraction (CCF) fields.
#' @based The logarithm base used in shannon, Default is exp(1).
#' @param bin_width optional argument of the width of bin which was split. Default=0.1.
#' @param start the start number of bin. Default=0.
#' @param end the end number of bin. Default=1.
#' @param adjusted optional argument to adjust ccf or vaf. Default=FALSE.
#' @param weighted optional argument to weight shannon entropy. Default=TRUE.
#' @param given_weights optional argument of the weight value. Default=NULL.
#'
#' @return Shannon index and Simpson_index
#' @export
#'
#' @examples
calcMutationEntropy <- function(mutation_data,
                               cancer_cell_fraction = FALSE,
                               based = exp(1),
                               nbins = 10,
                               start = 0,
                               end = 1,
                               adjusted = FALSE,
                               weighted = TRUE,
                               given_weights = NULL)
{
  # 针对单个样本进行ITH计算
  # code update from yiran.cai
  # ref：https://www.nature.com/articles/s41598-019-41098-0#Sec7

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
    dplyr::mutate(Tumor_VAF_backup = Tumor_VAF)
  # Adjusted
  max_vaf <- max(mutation_data$Tumor_VAF,na.rm = TRUE)
  if(adjusted){
    mutation_data <- mutation_data %>%
      dplyr::mutate( Tumor_VAF = Tumor_VAF/max_vaf)
  }

  bin_width = 1/nbins
  # Weighted
  if(weighted){
    if(!is.null(given_weights)){
      weights_vec <- given_weights
    }else{
      weights_vec <-
        seq(
          from=start+bin_width/2,
          to=end,
          by=bin_width
        )
    }
  }else{
    weights_vec <- rep(1,round(1/bin_width))
  }

  shannon_distribution <-
    data.frame(
      start = seq(from = start, to = end - bin_width, by=bin_width),
      end   = seq(from = start + bin_width, to = end, by=bin_width)
    )
  shannon_distribution <- shannon_distribution %>%
    dplyr::group_by(start,end) %>%
    dplyr::summarise(
      counts =
        mutation_data %>%
        filter(Tumor_VAF > start, Tumor_VAF <= end) %>%
        nrow(),
      .groups = "drop") %>%
    dplyr::mutate(pi = counts/sum(counts)) %>%
    dplyr::mutate(weight = weights_vec) %>%
    dplyr::mutate(
      pilnpi =
        case_when(
          pi != 0 ~ pi*log(pi, base = based),
          pi == 0 ~ 0
        )
    ) %>%
    dplyr::mutate(fipilnpi = pilnpi*weight)
  # Shannon Index
  hindex <- -sum(shannon_distribution$fipilnpi,na.rm = TRUE)
  shannon_index <- hindex
  # Simpson Index: is often as its complement (1-D)
  simpson_index <- 1-sum(shannon_distribution$pi^2,na.rm = TRUE)

  return(data.frame("Shannon_index" = round(shannon_index,4),
                    "Simpson_index" = round(simpson_index,4)))

}


#' Title
#'
#' @param mutation_data signleregional mutation data，mandatory fields: Tumor_VAF or CCF (if cancer_cell_fraction is TRUE )
#' @param cancer_cell_fraction FALSE. Is data have cancer_cell_fraction (CCF) fields.
#' @based The logarithm base used in shannon, Default is exp(1).
#' @param bin_width optional argument of the width of bin which was split. Default=0.1.
#' @param start the start number of bin. Default=0.
#' @param end the end number of bin. Default=1.
#' @param adjusted optional argument to adjust ccf or vaf. Default=FALSE.
#' @param weighted optional argument to weight shannon entropy. Default=TRUE.
#' @param given_weights optional argument of the weight value. Default=NULL.
#'
#' @return
#' @export
#'
#' @examples
lpy_calcMutationEntropy <- function(mutation_data,
                                   cancer_cell_fraction = FALSE,
                                   based = exp(1),
                                   nbins = 10,
                                   start = 0,
                                   end = 1,
                                   adjusted = FALSE,
                                   weighted = TRUE,
                                   given_weights = NULL,
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

    sid <- sids[i]

    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate Shannon entropy, Sample ID:", sid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    p_index <- mutation_data %>%
      dplyr::filter(SampleID == sid) %>%
      calcMutationEntropy(cancer_cell_fraction = cancer_cell_fraction,
                         based = based,
                         nbins = nbins,
                         start = start,
                         end = end,
                         adjusted = adjusted,
                         weighted = weighted,
                         given_weights = given_weights) %>%
      dplyr::mutate("SampleID" = sid) %>%
      dplyr::select(SampleID,Shannon_index,Simpson_index)


    return(p_index)
  }))
  out <- out %>%
    dplyr::mutate(Evenness_index = round(Shannon_index/max(Shannon_index),4)) %>%
    dplyr::select(SampleID, `Shannon entropy` = Shannon_index)
  return(out)
}
