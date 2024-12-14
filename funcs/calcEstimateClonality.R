
#' @title  Infer the ITH value by estimateclonality algorithm
#' @description Infer the ITH value by estimateclonality algorithm based on mutation data and segment file
#'
#' @param mutation_file mutation file, a data frame with "Patient","Chr","Start_position","End_position","Reference","Alternate","Variant_freq","Ref_freq","Hugo_Symbol","Variant_Classification" columns.
#' @param seg_file segment file, an output file from ASCAT.
#' @param out_dir
#' @param data_type
#'
#' @return
#' A data frame with three columns:
#' @returns sample_id:  Tumor samples id.
#' @returns comb.timing: The comb.timing type.
#' @returns estimateclonality: The ratio of comb.timing type.

#' @export
#'
#' @examples
calcEstimateClonality <- function(mutation_file,
                                     seg_file,
                                     out_dir,
                                     data_type = '',
                                  show_progress = TRUE){

  # 针对整个样本集进行ITH计算
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(data.table))
  # source("R/ith_by_earlylate.R")
  # pkgload::load_all()
  ith_index <- data.frame()
  mutation_data <- data.table::fread(mutation_file)
  if (!"Patient" %in% colnames(mutation_data) ) {
    message("The 'Patient' variable should be in the mutation data")
    stop(message(paste("Currently, mutation data missing the Patient variable!")))
  }
  mutation_samples <- unique(mutation_data$Patient)

  # load(seg_file)
  load_rdata <- function(filename){
    #loads an RData file, and returns it
    load(filename)
    get(ls()[ls() != "filename"])
  }
  seg_list <- load_rdata(seg_file)
  segments <- seg_list$segments
  if (!"SampleID" %in% colnames(segments) ) {
    message("The 'SampleID' variable should be in the segments data")
    stop(message(paste("Currently, segments data missing the SampleID variable!")))
  }
  segment_samples <- unique(segments$SampleID)
  common_samples <- intersect(mutation_samples,segment_samples)

  cat(paste("\nThere are ", length(common_samples), " patients with both copy number and mutation data. \nOnly these will be used\n"))

  if(length(common_samples) == 0){
    message("There are 0 patients with both copy number and mutation data")
    stop(message(paste("Currently, no patients with both copy number and mutation data!")))
  }else{
    n <- length(common_samples)
    # Create a Progress object
    progress <- shiny::Progress$new()
    # Make sure it closes when we exit this reactive, even if there's an error
    on.exit(progress$close())
    progress$set(message = "ITH evaluation: ", value = 0)

    ith_index <- do.call(rbind,lapply(seq(1,length(common_samples)),function(i){

      sample_name <- common_samples[i]

      if (show_progress) {
        update_modal_progress(
          value = i / n,
          text = paste("Calculate pLM, Sample ID:", sample_name, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
        )
      }
      # browser()
      message("The sample ",i," is: ",sample_name)
      calcEarlylate(mutation_file = mutation_file,
                       seg_file = seg_file,
                       sample_name = sample_name,
                       out_dir = out_dir,
                       data_type = '')

      if(!file.exists(paste0(out_dir,sample_name,"/",sample_name,".earlylate.tsv"))){
        message("EstimateClonality failed!")
        stop(message(paste("Currently, no file was printed from EstimateClonality!")))
      }else{
        tmp_out <- data.table::fread(paste0(out_dir,sample_name,"/",sample_name,".earlylate.tsv"))
        tmp_sta <- tmp_out %>%
          dplyr::filter(!is.na(comb.timing)) %>%
          dplyr::group_by(comb.timing) %>%
          dplyr::summarise(ncount = n(),
                           percent = n()/nrow(.)) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(sample_id = sample_name)
        if(nrow(tmp_sta) == 1){
          if(tmp_sta$comb.timing == 'Early') {
            tmp_sta <- tmp_sta %>%
              dplyr::bind_rows(data.frame(comb.timing = 'Late',
                                          ncount = 0,
                                          percent = 0,
                                          sample_id = sample_name))
          }else{
            tmp_sta <- tmp_sta %>%
              dplyr::bind_rows(data.frame(comb.timing = 'Early',
                                          ncount = 0,
                                          percent = 0,
                                          sample_id = sample_name))
          }
        }
        tmp_index <- tmp_sta %>%
          dplyr::select(SampleID = sample_id,
                        comb.timing,
                        estimateclonality = percent) %>%
          # dplyr::mutate(estimateclonality = round(estimateclonality,4)) %>%
          dplyr::mutate(estimateclonality = ifelse(estimateclonality >0.0001,
                                                   round(estimateclonality,4),
                                                   format(estimateclonality, scientific = T))) %>%
          tidyr::pivot_wider(id_cols = 'SampleID',
                             names_from = "comb.timing",
                             values_from = 'estimateclonality')
      }
      # delete folder and file in the folder
      unlink(paste0(out_dir,sample_name),recursive = T)


      # Increment the progress bar, and update the detail text.
      progress$inc(1/n, detail = sample_name)
      # Pause for 0.1 seconds to simulate a long computation.
      Sys.sleep(0.1)

      return(tmp_index)
    }))
  }

  return(ith_index)

}


#' Title
#'
#' @param mutation_file
#' @param seg_file
#' @param sample_name
#' @param out_dir
#' @param data_type
#'
#' @return
#' @export
#'
#' @examples
calcEarlylate <- function(mutation_file,
                          seg_file,
                          sample_name,
                          out_dir,
                          data_type = ''){
  suppressPackageStartupMessages(library(magrittr))
  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(data.table))
  # suppressPackageStartupMessages(library(EstimateClonality))
  # 该方法每次只分析一个样本的数据
  # source("./R/EstimateClonality/ccf.wrapper.r")
  # source("./R/EstimateClonality/earlyORlate.R")
  # source("./R/EstimateClonality/GD.functions.R")
  # source("./R/EstimateClonality/main.functions.r")
  # source("./R/EstimateClonality/utils.R")
  # pkgload::load_all()

  # out_dir: "./results/b_ascat/b01_ezascat2estimateclonality/"
  # mutation_file: paste0("./results/b_ascat/b01_ezascat2estimateclonality/",sample_name,"_mutation.txt")
  # seg_file: paste0("./results/b_ascat/b01_ezascat2estimateclonality/",sample_name,"_seg.Rdata")
  clonality.estimation(mutation.table.loc = mutation_file, # filename with path
                       seg.mat.loc = seg_file, # filename with path
                       data.type = data_type,
                       TCGA.barcode = sample_name, #
                       ANALYSIS.DIR = out_dir, # 输出文件名：sample_name.earlylate.tsv
                       sub.clonal.cut.off = 1,
                       min.var.prop = 0.05,
                       min.alt.reads = 5,
                       min.depth = 15,
                       plotting = F)
}
