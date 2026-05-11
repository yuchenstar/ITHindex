

# 2023-10-20
load_rdata <- function(filename){
  #loads an RData file, and returns it
  load(filename)
  get(ls()[ls() != "filename"])
}
# 2023-10-20
text_to_numeric <- function(text) {
  text <- gsub(" ", "", text)
  split <- strsplit(text, ",", fixed = FALSE)[[1]]
  as.numeric(split)
}

func_2zeroone <- function(x, ...){
  #####
  # transform vector to 0-1
  #####
  (x-min(x,na.rm = T, ...))/(max(x,na.rm = T, ...)-min(x,na.rm = T, ...))
}


densityPlot <- function(data){

  p_list <- list()
  # data <- ith_index
  if(ncol(data)>=2){
    idxs <- colnames(data)[-1]
    for (idx in idxs) {
      plot_data <- data %>%
        dplyr::select(colnames(data)[1],idx)
      colnames(plot_data)[2] <- "ith_value"
      p_out <- plot_data %>%
        dplyr::filter(!is.na(ith_value)) %>%
        ggplot(aes(x = ith_value)) +
        # ..density..
        geom_histogram(aes(y = after_stat(density)), colour = "black", fill = "white") +
        geom_density(alpha = .2, fill = "#00A064") +
        xlab(idx) +
        theme_classic() +
        ggtitle(label = paste0("Density plot of ITH index (", idx, ")")) +
        theme(
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          plot.title = element_text(size = 18, face = "bold")
        )
      p_list[[idx]] <- p_out
    }
  }
  return(p_list)
}


get_density <- function(x, y, ...) {
  dens <- MASS::kde2d(x, y, ...)
  ix <- findInterval(x, dens$x)
  iy <- findInterval(y, dens$y)
  ii <- cbind(ix, iy)
  return(dens$z[ii])
}

corrPlot <- function(data){

  p_list <- list()
  # data <- ith_index
  if(ncol(data)>=3){
    idxs <- colnames(data)[-1]
    for (i in seq_len(length(idxs)-1)) {
      for (j in seq(i+1,length(idxs))) {
        x_idx <- idxs[i]
        y_idx <- idxs[j]
        plot_data <- data %>%
          dplyr::select(colnames(data)[1],xvar = x_idx,yvar = y_idx)

        p_out <- plot_data%>%
          mutate(dens = get_density(xvar, yvar, n = 1000))%>%
          ggplot( aes(x = xvar, y = yvar)) +
          geom_point(aes(color = dens), size = 1) +
          scale_color_viridis(option = "turbo") +  # turbo色块
          geom_smooth(se = F, color = "black") +
          # cowplot::theme_cowplot() +
          stat_cor(method = "spearman", cor.coef.name = "cor",
                   label.x.npc = 0.05, label.y.npc = 0.1)+
          xlab(x_idx) +
          ylab(y_idx) +
          theme_classic() +
          theme(
            legend.position="none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text = element_text(size = 18),
            axis.title = element_text(size = 18),
            # plot.title = element_text(size = 22, face = "bold")
          )
        p_list[[paste0(x_idx,"@",y_idx)]] <- p_out
      }
    }
  }

  p_out <- p_null
  if(length(p_list) != 0){
    if(length(p_list) == 1) nr = nc = 1
    if(length(p_list) == 2) nr = 1;nc = 2
    if(length(p_list) >= 3) nr = 2;nc = ceiling(length(p_list)/2)
    p_out <- ggpubr::ggarrange(plotlist = p_list,nrow = nr,ncol = nc)
  }
  return(p_out)
}


barPlot <- function(data){

  p_list <- list()
  # data <- ith_index
  if(ncol(data)>=2){
    idxs <- colnames(data)[-1]
    for (idx in idxs) {
      plot_data <- data %>%
        dplyr::select(pid = colnames(.)[1],idx)
      colnames(plot_data)[2] <- "ith_value"
      p_out <- plot_data %>%
        dplyr::filter(!is.na(ith_value)) %>%
        dplyr::arrange(desc(ith_value)) %>%
        dplyr::mutate(pid = factor(pid,levels = pid)) %>%
        ggplot() +
        geom_bar(aes(x = pid,y = ith_value),stat="identity")+
        xlab("") +
        ylab(idx) +
        theme_classic() +
        ggtitle(label = paste0("Barplot plot of ITH index (", idx, ")")) +
        theme(
          axis.text = element_text(size = 18),
          axis.title = element_text(size = 18),
          # axis.text.x = element_text(angle = 45,hjust = 1,vjust = 1),
          axis.text.x = element_blank(),
          plot.title = element_text(size = 18, face = "bold")
        )
      p_list[[idx]] <- p_out
    }
  }
  return(p_list)
}


#' 过滤单样本患者并警告
#' @param df 数据框，必须包含 patient_id 列
#' @param patient_col 患者ID所在的列名，默认为 "patient_id"
filter_single_sample_patients <- function(
    df,
    patient_col = "PatientID",
    sample_col = "SampleID")
  {
  library(dplyr)
  # 1. 计算每个患者的样本数量
  patient_counts <- df %>%
    dplyr::distinct(!!sym(patient_col),!!sym(sample_col))%>%
    group_by(across(all_of(patient_col))) %>%
    summarise(sample_count = n(), .groups = "drop")

  # 2. 识别只有 1 个样本的患者
  single_sample_patients <- patient_counts %>%
    filter(sample_count == 1) %>%
    pull(!!sym(patient_col))

  # 3. 如果存在单样本患者，执行删除和告警
  if (length(single_sample_patients) > 0) {
    num_removed <- length(single_sample_patients)

    # 触发 shinyalert 弹窗 (英文提示)
    shinyalert::shinyalert(
      title = "Single-Sample Patients Removed",
      text = sprintf(
        "%d patient(s) (%s) were removed because they have only one sample, which is insufficient for multi-region heterogeneity analysis.",
        num_removed, paste(single_sample_patients, collapse = ", ")
      ),
      type = "warning"
    )

    # 执行过滤：只保留样本数 > 1 的患者数据
    df_filtered <- df %>%
      filter(!(!!sym(patient_col) %in% single_sample_patients))

    return(df_filtered)
  }

  # 如果没有单样本患者，原样返回
  return(df)
}
