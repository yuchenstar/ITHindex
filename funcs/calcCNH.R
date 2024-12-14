## Paper: Chromosomal copy number heterogeneity predicts survival rates across cancers
## Source:  https://github.com/dmmiedema/CNH
## 本Code使用 https://www.codeconvert.ai/matlab-to-r-converter 将原matlab脚本CNH.m转换为R脚本CNH.R，edited by WenchuanXie@20240318.

## MATLAB function to calculate copy number intra-tumor heterogeneity (CNH) from a single copy number measurement.
# In this method, relative segmented copy numbers are transformed to absolute copy numbers and the distance
# to integer values is measured for each segment. This procedure is
# performed for either a range of ploidies/purities or for fixed
# ploidy/purity, in case either or both quantities are known from previous
# measurements.
# Copy number heterogeneity (CNH) is defined as the minimum average distance of
# segments to the closest integer value, weighted by segment length.
# In addition to CNH, this function returns the ploidy and purity
# corresponding to the inferred CNH.

## Input
# 1st input argument:   (seg_val)       values of relative copy numbers per segment, vector of size Nx1
# 2nd input argument:   (seg_len)       segment lengths, vector of size Nx1
# 3th input argument:   (ploidy)        tumour ploidy, use empty vector ( = [] ) for grid search
# 4th input argument:   (purity)        sample purity, use empty vector ( = [] ) for grid search

## Output
# 1st output argument:  (CNH_out)       inferred CNH
# 2nd output argument:  (ploidy_out)    inferred ploidy for empty input ploidy, otherwise same as input ploidy.
# 3th output argument:  (purity_out)    inferred purity for empty input purity, otherwise same as input purity.

##  Main function
calcCNH <- function(seg_val, seg_len, ploidy = NULL, purity = NULL) {
  # check if input seg_val and seg_len are vectors of equal size
  if (!(is.data.frame(seg_val) && is.data.frame(seg_len) && nrow(seg_val) == nrow(seg_len) && dim(seg_val)[2] == 1)) {
    stop("Segment values (1st input argument) and segment lengths (second input argument) appear not to be column vectors of equal length")
  }
  # browser()
  # check if ploidy is scalar argument or empty
  # if (is.na(ploidy)||!(is.numeric(ploidy) && length(ploidy) == 1 && ploidy > 0)) {
  #   stop("Ploidy is not a positive scalar or empty")
  # }
  # check if purity is a scalar between 0 and 1 argument or empty
  # if (is.na(purity)||!(is.numeric(purity) && length(purity) == 1 && purity > 0 && purity <= 1)) {
  #   stop("Purity is not a positive scalar or empty")
  # }
  # specify default range of ploidy for grid search, if input ploidy is empty
  if (is.null(ploidy)) {
    ploidy <- seq(1.5, 5, 0.01)  # tumor ploidy
  }
  # specify default range of purity purity for grid search, if input purity is empty
  if (is.null(purity)) {
    purity <- seq(0.2, 1, 0.01)  # tumor purity: standard range
  }
  # get number of ploidies and purities for grid search
  Nploidy <- length(ploidy)
  Npurity <- length(purity)
  # initialize vectors a1 and a2 from all combinations of ploidy and purity, for the transformation of
  # measured relative copy number profile (seg_val) to absolute values (q) using
  # q = seg_val * a1 + a2.
  a1 <- rep(0, Nploidy * Npurity)
  a2 <- rep(0, Nploidy * Npurity)
  purity_all <- rep(0, Nploidy * Npurity)  # vector that contains all purities used in 2D grid search
  ploidy_all <- rep(0, Nploidy * Npurity)  # vector that contains all ploidies used in 2D grid search
  for (i in 1:Nploidy) {
    # i= 1
    index = ((i - 1) * Npurity + 1):(i * Npurity)
    a1[index] <- (purity * ploidy[i] + 2 * (1 - purity)) / purity
    a2[index] <- -2 * (1 - purity) / purity
    purity_all[index] <- purity
    ploidy_all[index] <- rep(ploidy[i], Npurity)
  }
  # iniatilize output: CNH_out, ploidy_out and purity_out
  CNH_out <- 1
  purity_out <- 0
  ploidy_out <- 0
  # grid search over all ploidies and purities to infer CNH

  if(F){
    start_time <- Sys.time() # 记录初始时间
    for (i in 1:(Nploidy * Npurity)) {
      # transform relative copy numbers to absolute numbers
      q <- a1[i] * seg_val + a2[i]
      # measure distance to closest integer value of each segment
      # 取模运算
      q_dist_down <- q %% 1
      q_dist_up <- 1 - (q %% 1)
      q_dist_min <- min(q_dist_up, q_dist_down)
      # calculate the mean distance of segments to integer values,
      # weighted by segment length
      CNHnew <- sum(q_dist_min * seg_len) / sum(seg_len)
      # if the new weighted distance to integers is smaller than any
      # previously calculated, replace CNH_out, ploidy_out and purity_out with the new values.
      if (CNHnew < CNH_out) {
        CNH_out <- CNHnew
        purity_out <- purity_all[i]
        ploidy_out <- ploidy_all[i]
      }
    }
    out_data <- data.frame("CNH" = CNH_out,
                           "Purity" = purity_out,
                           "Ploidy" = ploidy_out)
    end_time <- Sys.time() # 记录终止时间
    # message("Time cost1: ",end_time - start_time) # 计算时间差
  }
  if(F){
    start_time <- Sys.time() # 记录初始时间
    results <- foreach(i = seq_len((Nploidy * Npurity)), .combine = 'rbind') %dopar% {
      # transform relative copy numbers to absolute numbers
      q <- a1[i] * seg_val + a2[i]
      # measure distance to closest integer value of each segment
      # 取模运算
      q_dist_down <- q %% 1
      q_dist_up <- 1 - (q %% 1)
      q_dist_min <- min(q_dist_up, q_dist_down)
      # calculate the mean distance of segments to integer values,
      # weighted by segment length
      CNHnew <- sum(q_dist_min * seg_len) / sum(seg_len)
      # if the new weighted distance to integers is smaller than any
      # previously calculated, replace CNH_out, ploidy_out and purity_out with the new values.
      if (CNHnew < CNH_out) {
        CNH_out <- CNHnew
        # purity_out <- purity_all[i]
        # ploidy_out <- ploidy_all[i]
        out <- data.frame("CNH" = CNH_out,
                          "Purity" = purity_all[i],
                          "Ploidy" = ploidy_all[i])
      }


    }
    out_data <- results %>%
      dplyr::arrange(CNH) %>%
      dplyr::slice(1)
    end_time <- Sys.time() # 记录终止时间
    message("Time cost2: ",end_time - start_time) # 计算时间差
  }

  # start_time <- Sys.time() # 记录初始时间
  CNH_out <- 1
  results <- future_lapply(seq_len((Nploidy * Npurity)),function(i){
    # transform relative copy numbers to absolute numbers
    q <- a1[i] * seg_val + a2[i]
    # measure distance to closest integer value of each segment
    # 取模运算
    q_dist_down <- q %% 1
    q_dist_up <- 1 - (q %% 1)
    q_dist_min <- min(q_dist_up, q_dist_down)
    # calculate the mean distance of segments to integer values,
    # weighted by segment length
    CNHnew <- sum(q_dist_min * seg_len) / sum(seg_len)
    # if the new weighted distance to integers is smaller than any
    # previously calculated, replace CNH_out, ploidy_out and purity_out with the new values.
    if (CNHnew < CNH_out) {
      CNH_out <- CNHnew
      # purity_out <- purity_all[i]
      # ploidy_out <- ploidy_all[i]
      out<- data.frame("CNH" = CNH_out,
                       "Purity" = purity_all[i],
                       "Ploidy" = ploidy_all[i])
    }

  })
  out_data <- do.call(rbind.data.frame, results)
  # out_data <- data.frame(matrix(unlist(results), nrow=length(results), byrow=TRUE))
  out_data <- out_data %>%
    dplyr::arrange(CNH) %>%
    dplyr::slice(1)
  # end_time <- Sys.time() # 记录终止时间
  # message("Time cost3: ",end_time - start_time) # 计算时间差

  return(out_data)
}



#' Title
#'
#' @param seg_data a segment dataframe, mandatory fields: SampleID Start End Segment_Mean
#' @param pp_data a purity and ploidy dataframe, mandatory fields: SampleID Purity Ploidy
#'
#' @return
#' @export
#'
#' @examples
lpy_calcCNH <- function(seg_data, pp_data = NULL, show_progress = TRUE){

  data_cols <- c("SampleID", "Start", "End", "Segment_Mean")
  if (!is.data.frame(seg_data) | length(grep(FALSE, data_cols %in% colnames(seg_data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  if (!(is.numeric(seg_data$Start) & is.numeric(seg_data$End) & is.numeric(seg_data$Segment_Mean))) {
    print(paste("Error, the type of this columns: ",
                paste(c("Start", "End", "Segment_Mean"), collapse=" "),
                "must be numeric!"))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  if(is.null(pp_data)){

    same_sids <- unique(seg_data$SampleID)
    pp_list <- list()
    for (sid in same_sids) {
      pp_list[[sid]][['ploidy']] <- seq(1.5, 5, 0.01)
      pp_list[[sid]][['purity']] <- seq(0.2, 1, 0.01)
    }

  }else{
    data_cols <- c("SampleID", "Ploidy", "Purity")
    if (!is.data.frame(pp_data) | length(grep(FALSE, data_cols %in% colnames(pp_data))) != 0) {
      print(paste("Error, data must be a dataframe, and contain the following columns: ",
                  paste(data_cols, collapse=" ")))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
    if (!(is.numeric(pp_data$Ploidy) & is.numeric(pp_data$Purity))) {
      print(paste("Error, the type of this columns: ",
                  paste(c("Ploidy", "Purity"), collapse=" "),
                  "must be numeric!"))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
    pp_data <- na.omit(pp_data)
    message("Only the ", nrow(pp_data)," samples have ploidy and purity, and will be calc the CNH!")

    same_sids <- intersect(unique(seg_data$SampleID),pp_data$SampleID)

    pp_list <- list()
    for (sid in same_sids) {
      pp_list[[sid]][['ploidy']] <- pp_data[pp_data$SampleID == sid,]$Ploidy
      pp_list[[sid]][['purity']] <- pp_data[pp_data$SampleID == sid,]$Purity
    }
  }

  if (length(same_sids) == 0) {
    print(paste("Error, no identical SampleID between segment data and purity and ploidy data!"))
    return(NULL)
  }
  n <- length(same_sids)
  list_cnh <- lapply(seq(1,length(same_sids)),function(i){

    sid = same_sids[i]
    message("Sample ID: ",sid)
    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate CNH, Sample ID:", sid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    seg_val_s <- seg_data %>%
      dplyr::mutate(Segment_Length = End-Start) %>%
      dplyr::filter(SampleID == sid)
    seg_val <- data.frame("seg_val" = 2^seg_val_s$Segment_Mean)
    seg_len <- data.frame("seg_len" = seg_val_s$Segment_Length)

    ploidy = pp_list[[sid]][['ploidy']]
    purity = pp_list[[sid]][['purity']]

    # 调用CNH函数时，ploidy和purity的值是一个数值，不是数值向量
    # 不输入时，CNH函数内置会使用如下参数值。
    # in_purity <- seq(0.2, 1, 0.01)
    # in_ploidy <- seq(1.5, 5, 0.01)
    # 在以上purity和ploidy的组合下，单个样本需要12S时间计算

    cnh <- calcCNH(seg_val = seg_val,seg_len = seg_len,ploidy = ploidy,purity = purity)
    cnh$SampleID = sid

    # update_modal_progress(
    #   value = i / n,
    #   text =  paste("Sample:", i, ": ", sid ,sprintf(" (%1.0f%%)", i/n*100))
    # )
    # progress$inc(1/n, detail = sid)
    # Sys.sleep(0.1)

    return(cnh)
  })

  # common_samples <- same_sids
  # n <- length(common_samples)
  # progress <- shiny::Progress$new()
  # on.exit(progress$close())
  # progress$set(message = "CNH calculation: ", value = 0)

  out <- do.call(rbind.data.frame, list_cnh)
  out <- out %>%
    # dplyr::mutate(CNH = ifelse(CNH>0.0001,round(CNH,4),format(CNH, scientific = T))) %>%
    dplyr::mutate(CNH = as.numeric(CNH)) %>%
    dplyr::select(SampleID,CNH,Purity,Ploidy)
  return(out)
}


#'
#' For sample RCN profile r finds CNH+ and the associated tumor purity, tumor ploidy
#'
#' \code{find_cnhplus} by searching over grid, finds value of CNH+ and associated tumor purity, tumor ploidy for a sample RCN.
#' It also makes possible to find CNH.
#'
#' @param grid, grid of purities, ploidies (matrix)
#' @param r sample RCN profile (vector)
#' @param w vector of segment widths
#' @param k how many candidate pairs of purity, ploidy for the solution to return
#' @param plus if True (default) CNH+ is solved for; if False, CNH
#' @return top k elements of the grid together with the associated values of the objective function kappa
#' @seealso \code{plot_profile}
#' @export
find_cnhplus2 = function(grid, r, w, k, plus = TRUE) {
  #
  tryCatch(
    {
      if (nrow(grid) == 0) {
        return()
      }
    },
    error = function(e) {
      message('    Error: grid is missing!')
    }
  )
  #
  # if (plus == TRUE) {
  #   grid = make_grid_plus(grid=grid, r=r)
  # }
  grid = make_grid_plus(grid=grid, r=r)
  if (is.numeric(grid)) grid <- matrix(grid,ncol = 2)
  # 当对每个样本给定一个purity和ploidy时，可能其不满足分析：计算的ACN值有负数，导致被删除
  # 需要重新赋予一个区间，重新检查。
  if (!(is.matrix(grid) & nrow(grid) != 0)) {
    message("Make a new grid of purity and ploidy!")
    grid = make_grid(purity = seq(0.2, 1, 0.01), ploidy = seq(1.5, 5, 0.01))
  }
  #
  kappas = apply(grid, 1, kappa, r=r, w=w)
  return(topk(grid = grid, kappas = kappas, k = k))
  #
}


lpy_calcCNHplus <- function(seg_data, pp_data = NULL, plus = TRUE, show_progress = TRUE){

  data_cols <- c("SampleID", "Start", "End", "Segment_Mean")
  if (!is.data.frame(seg_data) | length(grep(FALSE, data_cols %in% colnames(seg_data))) != 0) {
    print(paste("Error, data must be a dataframe, and contain the following columns: ",
                paste(data_cols, collapse=" ")))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  if (!(is.numeric(seg_data$Start) & is.numeric(seg_data$End) & is.numeric(seg_data$Segment_Mean))) {
    print(paste("Error, the type of this columns: ",
                paste(c("Start", "End", "Segment_Mean"), collapse=" "),
                "must be numeric!"))

    # message("Error, data must be a dataframe, and contain the following columns: ",
    #         paste(data_cols, collapse=" "))
    # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

    return(NULL)
  }
  if(is.null(pp_data)){

    same_sids <- unique(seg_data$SampleID)
    pp_list <- list()
    for (sid in same_sids) {
      pp_list[[sid]][['ploidy']] <- seq(1.5, 5, 0.01)
      pp_list[[sid]][['purity']] <- seq(0.2, 1, 0.01)
    }

  }else{
    data_cols <- c("SampleID", "Ploidy", "Purity")
    if (!is.data.frame(pp_data) | length(grep(FALSE, data_cols %in% colnames(pp_data))) != 0) {
      print(paste("Error, data must be a dataframe, and contain the following columns: ",
                  paste(data_cols, collapse=" ")))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }
    if (!(is.numeric(pp_data$Ploidy) & is.numeric(pp_data$Purity))) {
      print(paste("Error, the type of this columns: ",
                  paste(c("Ploidy", "Purity"), collapse=" "),
                  "must be numeric!"))

      # message("Error, data must be a dataframe, and contain the following columns: ",
      #         paste(data_cols, collapse=" "))
      # stop(message(paste0("Error, The mandatory fields: ",paste(data_cols, collapse=" "),"!")))

      return(NULL)
    }

    pp_data <- na.omit(pp_data)
    message("Only the ", nrow(pp_data)," samples have ploidy and purity, and will be calc the CNHplus!")

    same_sids <- intersect(seg_data$SampleID,pp_data$SampleID)

    pp_list <- list()
    for (sid in same_sids) {
      pp_list[[sid]][['ploidy']] <- pp_data[pp_data$SampleID == sid,]$Ploidy
      pp_list[[sid]][['purity']] <- pp_data[pp_data$SampleID == sid,]$Purity
    }
  }
  n <- length(same_sids)
  list_cnhplus <- lapply(seq(1,length(same_sids)),function(i){

    # Sample ID: TCGA-4C-A93U-01
    # Sample ID: TCGA-BJ-A0YZ-01
    # Sample ID: TCGA-BJ-A0Z0-01
    # Sample ID: TCGA-BJ-A0Z2-01
    # sid = 'TCGA-BJ-A0Z2-01'

    sid = same_sids[i]
    message("Sample ID: ",sid)
    if (show_progress) {
      update_modal_progress(
        value = i / n,
        text = paste("Calculate CNHplus, Sample ID:", sid, " (", i, "/",n,", ", sprintf(" %1.0f%%)", i / n * 100))
      )
    }

    seg_val_s <- seg_data %>%
      dplyr::mutate(Segment_Length = End-Start) %>%
      dplyr::filter(SampleID == sid)
    r <- 2^seg_val_s$Segment_Mean
    w <- seg_val_s$Segment_Length

    grid = make_grid(purity = pp_list[[sid]][['purity']], ploidy = pp_list[[sid]][['ploidy']])
    cnhplus = find_cnhplus2(grid,
                           r = r,
                           w = w,
                           k=1,
                           plus = plus)
    cnhplus <- cnhplus %>%
      dplyr::mutate(SampleID = sid) %>%
      dplyr::rename(CNHplus = kappa) %>%
      dplyr::mutate(CNHplus = ifelse(CNHplus >0.0001,round(CNHplus,4),format(CNHplus, scientific = T))) %>%
      dplyr::select(SampleID,CNHplus,Purity = purity,Ploidy = ploidy)

    # update_modal_progress(
    #   value = i / n,
    #   text =  paste("Sample:", i, ": ", sid ,sprintf(" (%1.0f%%)", i/n*100))
    # )
    # progress$inc(1/n, detail = sid)
    # Sys.sleep(0.1)

    return(cnhplus)
  })

  # common_samples <- same_sids
  # n <- length(common_samples)
  # progress <- shiny::Progress$new()
  # on.exit(progress$close())
  # progress$set(message = "CNHplus calculation: ", value = 0)

  out <- do.call(rbind.data.frame, list_cnhplus)
  return(out)
}


