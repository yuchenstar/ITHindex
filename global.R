#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : global.R
# @License : Copyright(C), X
# @Author  : wenchuan.xie
# @Time    : 2023-10-16
# @IDE     : RStudio
# @Desc    : 项目自定义函数集合
#===============================================================================


if(F){
  # 安装的devtoolset是在 /opt/rh 目录下的
  # gcc --version
  # mv /usr/bin/gcc /usr/bin/gcc-4.8.5
  # ln -s /opt/rh/devtoolset-8/root/bin/gcc /usr/bin/gcc
  # mv /usr/bin/g++ /usr/bin/g++-4.8.5
  # ln -s /opt/rh/devtoolset-8/root/bin/g++ /usr/bin/g++
  # gcc --version
  # g++ --version
}
# 输出文件前缀
ith_prefix <- 'ITHindex'

# 临时文件目录
ith_path <- "data/tmp/"

# Increase band width for shiny to handle bigger file
options(shiny.maxRequestSize=100*1024^2)

# Only for CNH.
# library(future.apply)
# plan(multisession(workers = 6));
# options(future.globals.maxSize = 100 * 1024^4);
# sprintf("FUTURE CURRENT WORKERS = %s", nbrOfWorkers());

library(shiny)
library(shinythemes)     # install.packages("shinythemes")， 105环境是apper用户下安装
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(shinysky)     # devtools::install_github("AnalytixWare/ShinySky")
library(shinybusy)
library(shinycookie)  # devtools::install_github("colearendt/shinycookie")
library(shinydisconnect)
library(shinyvalidate)
library(shinyjs)      # 导致报错： Error: colourInput() has been moved to the 'colourpicker' package.
library(shinycssloaders)
library(shinyThings) # devtools::install_github("gadenbuie/shinyThings")，radioSwitchButtons
# library(dipsaus) # for actionButtonStyled
library(prompter)    # add_prompt
library(shinyBS)     # bsModal
library(DT)
library(mailtoR)

library(markdown)
library(rmarkdown)
library(MASS)
library(rbbt) # remotes::install_github("paleolimbot/rbbt")


library(viridis)
library(ggplot2)
library(ggpubr)
library(ggbreak) # remotes::install_github("YuLab-SMU/ggbreak")  remotes::install_github("YuLab-SMU/aplot")
library(naniar)
library(openxlsx)
library(vegan)
library(spatstat)
library(parallel)

library(rintrojs)
library(dplyr)
library(magrittr)
library(broom)
library(Hmisc)
library(tibble)
library(tidyr)
library(boot)


# library(CNHplus) # devtools::install_github("grendar/CNHplus")



# EstimateClonality
source("R/EstimateClonality/ccf.wrapper.r")
source("R/EstimateClonality/earlyORlate.R")
source("R/EstimateClonality/GD.functions.R")
source("R/EstimateClonality/main.functions.r")
source("R/EstimateClonality/utils.R")

# CNHplus
source("R/CNHplus/check_q.R")
source("R/CNHplus/kappa.R")
source("R/CNHplus/kappa_q.R")
source("R/CNHplus/make_grid.R")
source("R/CNHplus/make_grid_plus.R")
source("R/CNHplus/r2q.R")
source("R/CNHplus/topk.R")


source("funcs/calcJaccardSimilarity.R")    # multi-regional mutation
source("funcs/calcMATH.R")                 # mutation
source("funcs/calcPClonality.R")           # CCF
source("funcs/calcMutationEntropy.R")      # mutation or CCF

source("funcs/calcEstimateClonality.R") # mutation and segment

source("funcs/calcCNH.R")               # segment CBS
source("funcs/snpDiv.R")                # segment LRR and BAF
source("funcs/cbsDiv.R")                # segment CBS

# BioQC::entropyDiversity()
source("funcs/calcExprEntropy.R")       # Array, bulkRNAseq(normalized: TPM) or Proteome (normalized)
source("funcs/calcPcaDiversity.R")      # scRNAseq(normalized: TPM) or multi-regional bulkRNAseq(normalized: TPM, need Coordinate)
source("funcs/calcMPD.R")       # multi-regional bulkRNAseq/proteome


source("funcs/calcMethylEntropy.R")     # Methylation
source("funcs/calcSD.R")                # bulkRNAseq or Methylation


# load ui/server from each tab
source('R/quantification_2.0.R')
source('R/data.R')
source('R/about.R')
source("R/utils.R")

# example data
demo_file_mutation_multiregional <- "data/example/multiregional_mutation.csv"
demo_file_mutation_singleregional <- "data/example/singleregional_mutation.csv"
demo_file_ccf <-"data/example/cancer_cell_fraction.csv"
demo_file_segment <- "data/example/cnh_segments.tsv"
demo_file_purity_ploidy <- "data/example/purity_ploidy.tsv"
demo_file_segment_logR <- "data/example/segment_div.txt"
## for EstimateClonality
demo_file_segment_nAnB <- "data/example/estimateclonality_segments.tsv"
demo_file_ec_segment <- "data/example/estimateclonality_segments.rdata"
demo_file_ec_mutation <- "data/example/estimateclonality_mutations.tsv"
demo_out_dir <- "data/tmp/"

demo_file_bulkRNAseq_TPM <- "data/example/bulkRNAseq_log2TPM.txt"

demo_file_bulkRNAseq_TPM_multiregional <- "data/example/multiregional_bulkRNAseq_TPM.txt"
demo_file_bulkRNAseq_coordinate_multiregional <- "data/example/multiregional_bulkRNAseq_Coordinates.txt"

demo_file_scRNAseq_TPM <- "data/example/scRNAseq_TPM.txt"
demo_file_scRNAseq_meta <- "data/example/scRNAseq_meta.txt"

demo_file_proteome_multiregional <- "data/example/multiregional_proteome.txt"
demo_file_proteome_meta_multiregional <- "data/example/multiregional_proteome_meta.txt"

demo_file_methylation <- "./data/example/methylation.txt"

# data for display in data tab
tcga_data <- data.table::fread("data/builtindata/ithindex_tcga.tsv") %>%
  # dplyr::select(-sample_id,-Late,-Early) %>%
  dplyr::rename(Cancer_type = cancer_type) %>%
  dplyr::mutate(Cancer_type = factor(Cancer_type)) %>%
  dplyr::select(PatientID,SampleID,Cancer_type,
                `Shannon entropy (mutation)` = `Shannon_entropy (mutation)`,MATH,
                `Shannon entropy (segment)` = `Shannon_entropy (segment)`, CNHplus,#CNH,
                `Shannon entropy (expression)` = `Shannon_entropy (expression)`,
                `Standard deviations (expression)` = `mSD (exprression)`,
                `Shannon entropy (methylation)` = `Shannon_entropy (methylation)`,
                `Standard deviations (methylation)` = `mSD (methylation)`
                # ,Early
                ,pLM = Late
                ) %>%
  dplyr::mutate_if(is.numeric,~round(.x,4))

# 全部指标scale到0-1后展示
p_tcga_box <- tcga_data %>%
  # dplyr::mutate(sMATH = func_2zeroone(MATH)) %>%
  dplyr::mutate_at(.vars = c('Shannon entropy (mutation)','MATH',
                             'Shannon entropy (segment)','CNHplus',#'CNH',
                             'Shannon entropy (expression)','Standard deviations (expression)',
                             'Shannon entropy (methylation)','Standard deviations (methylation)',
                             'pLM') ,.funs = func_2zeroone) %>%
  tidyr::pivot_longer(cols = c(`Shannon entropy (mutation)`,MATH,
                               `Shannon entropy (segment)`,CNHplus,#CNH,
                               `Shannon entropy (expression)`,`Standard deviations (expression)`,
                               `Shannon entropy (methylation)`,`Standard deviations (methylation)`,
                               pLM),
                      names_to = "ITHindex",values_to = "ITHvalue") %>%
  dplyr::mutate(ITHindex = factor(ITHindex,levels = c('Shannon entropy (mutation)','MATH',
                                                      'Shannon entropy (segment)','CNHplus',#'CNH',
                                                      'Shannon entropy (expression)','Standard deviations (expression)',
                                                      'Shannon entropy (methylation)','Standard deviations (methylation)',
                                                      'pLM'))) %>%
  dplyr::filter(!is.na(Cancer_type)) %>%
  ggplot(aes(x = Cancer_type, y = ITHvalue,fill = ITHindex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  # scale_y_break(c(2.5, 9)) +
  # ggbreak::scale_y_break(c(2.5, 9),scales = 2)+
  theme_classic()+
  scale_fill_manual(values = c("#E39E3F","#edc082",
                               "#005e32","#00AA5A",
                               "#4735d2","#8074e0",
                               "#91003F","#de0060",
                               '#b674e0',"#d7b3ee")) +
  xlab("") +
  ylab("ITH index") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1),
    # axis.text.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )

# 分开绘图在合并
p_tcga_mutation <- tcga_data %>%
  # dplyr::mutate(sMATH = func_2zeroone(MATH)) %>%
  tidyr::pivot_longer(cols = c(`Shannon entropy (mutation)`,MATH),
                      names_to = "ITHindex",values_to = "ITHvalue") %>%
  dplyr::mutate(ITHindex = factor(ITHindex,levels = c('Shannon entropy (mutation)',
                                                      'MATH'))) %>%
  dplyr::filter(!is.na(Cancer_type)) %>%
  ggplot(aes(x = Cancer_type, y = ITHvalue,fill = ITHindex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  # scale_y_break(c(2.5, 9)) +
  # ggbreak::scale_y_break(c(2.5, 9),scales = 2)+
  theme_classic()+
  scale_fill_manual(values = c("#00AA5A","#E16A86")) +
  xlab("") +
  ylab("ITH index") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1),
    # axis.text.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )
# p_tcga_mutation

p_tcga_segment <- tcga_data %>%
  dplyr::mutate(sMATH = func_2zeroone(MATH)) %>%
  tidyr::pivot_longer(cols = c(`Shannon entropy (segment)`,CNHplus),
                      names_to = "ITHindex",values_to = "ITHvalue") %>%
  dplyr::mutate(ITHindex = factor(ITHindex,levels = c('Shannon entropy (segment)',
                                                      'CNHplus'))) %>%
  dplyr::filter(!is.na(Cancer_type)) %>%
  ggplot(aes(x = Cancer_type, y = ITHvalue,fill = ITHindex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  # scale_y_break(c(2.5, 9)) +
  # ggbreak::scale_y_break(c(2.5, 9),scales = 2)+
  theme_classic()+
  scale_fill_manual(values = c("#00AA5A","#E16A86")) +
  xlab("") +
  ylab("ITH index") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1),
    # axis.text.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )
# p_tcga_segment

p_tcga_expression <- tcga_data %>%
  tidyr::pivot_longer(cols = c(`Shannon entropy (expression)`,`Standard deviations (expression)`),
                      names_to = "ITHindex",values_to = "ITHvalue") %>%
  dplyr::mutate(ITHindex = factor(ITHindex,levels = c('Shannon entropy (expression)',
                                                      'Standard deviations (expression)'))) %>%
  dplyr::filter(!is.na(Cancer_type)) %>%
  ggplot(aes(x = Cancer_type, y = ITHvalue,fill = ITHindex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  # scale_y_break(c(2.5, 9)) +
  # ggbreak::scale_y_break(c(5, 12),scales = 2)+
  theme_classic()+
  scale_fill_manual(values = c("#00AA5A","#E16A86")) +
  xlab("") +
  ylab("ITH index") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1),
    # axis.text.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )
# p_tcga_expression

p_tcga_methylation <- tcga_data %>%
  tidyr::pivot_longer(cols = c(`Shannon entropy (methylation)`,`Standard deviations (methylation)`),
                      names_to = "ITHindex",values_to = "ITHvalue") %>%
  dplyr::mutate(ITHindex = factor(ITHindex,levels = c('Shannon entropy (methylation)',
                                                      'Standard deviations (methylation)'))) %>%
  dplyr::filter(!is.na(Cancer_type)) %>%
  ggplot(aes(x = Cancer_type, y = ITHvalue,fill = ITHindex)) +
  stat_boxplot(geom ='errorbar', width = 0.6) +
  geom_boxplot(width = 0.6) +
  # scale_y_break(c(2.5, 9)) +
  # ggbreak::scale_y_break(c(5, 12),scales = 2)+
  theme_classic()+
  scale_fill_manual(values = c("#00AA5A","#E16A86")) +
  xlab("") +
  ylab("ITH index") +
  theme(
    legend.position = 'bottom',
    axis.text.x = element_text(angle = 90,hjust = 1,vjust = 1),
    # axis.text.x = element_blank(),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    plot.title = element_text(size = 18, face = "bold")
  )
# p_tcga_methylation


# 空白图
p_noinfo <- ggplot2::ggplot() +
  theme_void() +
  geom_text(aes(0, 0, label = "NOT APPLICABLE!"),
            colour = "grey", fontface = "bold", size = 8
  ) +
  xlab(NULL)

p_null <- ggplot2::ggplot() +
  theme_void() +
  geom_text(aes(0, 0, label = "NOT AVAILABLE!"),
            colour = "grey", fontface = "bold", size = 8
  ) +
  xlab(NULL)

p_less <- ggplot2::ggplot() +
  theme_void() +
  geom_text(aes(0, 0, label = "Less than 10 records,\nno plot!"),
            colour = "grey", fontface = "bold", size = 8
  ) +
  xlab(NULL)



# 2023-10-20 send plain email
# InfromMe <- function(info){
#   from <- "csanmum1986@163.com"
#   to <- "wenchuan.xie@gmail.com"
#   subject <- info$subject
#   body <- info$body
#   mailControl=list(smtpServer="serverinfo")
#   sendmailR::sendmail(from=from,to=to,subject=subject,msg=body,control=mailControl)
# }

# 创建许可证（执行一次）
# create_smtp_creds_key(
#   id = "gmail", # 帮助文档是以Gmail为例的，这里以Outlook为例。
#   user = "wenchuan.xie@gmail.com",  # 这里填入你的邮件地址
#   provider = "gmail",overwrite = T
# )
# view_credential_keys()


render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

