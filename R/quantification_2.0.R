# ===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : ith_quantification.R
# @License : Copyright(C), X
# @Author  : Wenchuan Xie
# @Time    : 2023-10-18
# @IDE     : RStudio
# @Desc    : ITH quantification use different algorithms
# ===============================================================================


# List contain ui and server of page
ith_quantification_page <- list()

quantification_page <- dashboardBody(
  # br(),
  # fluidRow(
  #   box(width = 12, collapsible = TRUE, title = "Note:", "")
  # ),
  id = "quantification_page_id",
  style = "color:#111111;background-color:#ffffff;",
  # STEP1 ----
  fluidRow(
    style = "margin-top:5%",
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE
      # ,status = "warning"
      , solidHeader = TRUE,
      title = strong("STEP1, Choose the data type you want to upload!",
        style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;", # display:block;padding:10px;position:relative;
      awesomeRadio(
        inputId = "ith_data_type",
        label = "Data type which your want to upload",
        inline = T,
        # choices = c("VAF", "CCF", "Expression", "Segment and Mutation"),
        choices = c(
          "Mutation (VAF/CCF)" = "VAF_CCF",
          "Copy Number Variation" = "Segment",
          "RNA Expression/Protein Abundance" = "Expression",
          # "Protein Abundance" = "Protein",
          "Methylation" = "Methylation"
        ),
        selected = NULL,
        status = "success",
        checkbox = TRUE
      )
    )
  ),
  # STEP2 ----
  fluidRow(
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE,
      solidHeader = TRUE,
      title = strong("STEP2, Choose an algorithm to quantify ITH!",
        style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;", # display:block;padding:10px;position:relative;
      awesomeRadio(
        inputId = "ith_algorithm",
        label = "Metric which you want to use",
        choices = "",
        selected = NULL,
        status = "success",
        checkbox = TRUE
      ),
      fluidRow(
        # 1.Jaccard Similarity
        # No parameters

        # 2.MATH
        conditionalPanel(
          condition = 'input.ith_algorithm == "MAD"',
          column(
            2,
            radioSwitchButtons(
              inputId = "mad_ccf",
              label = "Cancer Cell Fraction?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            )
          )
        ),
        # 3.Entorpy
        conditionalPanel(
          # https://microbeatic.wordpress.com/2012/01/26/diversity-index-shannon-indexshannon-weaver-index-h/
          condition = 'input.ith_algorithm == "mutationEntropy"',
          column(
            2,
            radioSwitchButtons(
              inputId = "entropy_ccf",
              label = "Cancer Cell Fraction?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            )
          ),
          column(
            4,
            sliderInput(
              inputId = "entropy_nbin",
              label = "Number of bins",
              value = 10,
              min = 10,
              max = 100, step = 5, width = "100%"
            )
          ),
          column(
            2,
            radioSwitchButtons(
              inputId = "entropy_adjusted",
              label = "Adjusted by max VAF/CCF",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            )
          ),
          column(
            4,
            radioSwitchButtons(
              inputId = "entropy_weighted",
              label = "Weighted to entropy",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            ),
            fluidRow(
              conditionalPanel(
                condition = 'input.entropy_weighted == "yes"',
                column(12,
                  style = "height:60px;",
                  textInput(
                    inputId = "entropy_weighted_value",
                    label = tags$span(
                      add_prompt(tags$span(icon(name = "circle-question")),
                        message = "Enter a vector weight separated by a comma, no mre than 10.",
                        position = "right"
                      ),
                      "Weights of entropy"
                    ),
                    placeholder = "Enter numeric separated by a comma..."
                  )
                )
              )
            )
          )
        ),
        # 4.Ratio
        conditionalPanel(
          condition = 'input.ith_algorithm == "Ratio"',
          column(
            6,
            sliderInput(
              inputId = "ratio_cutoff",
              label = "CCF cutoff that defined clonal",
              value = 0.8,
              min = 0.6,
              max = 0.99, step = 0.01, width = "100%"
            )
          )
        ),
        # 5. CNHplus
        # No parameters

        # 6. Shannon entropy (Segment)
        conditionalPanel(
          condition = 'input.ith_algorithm == "segmentEntropy"',
          column(
            3,
            sliderInput(
              inputId = "segment_entropy_minprobes",
              label = "Minimum number of probes for a segment",
              value = 100,
              min = 100,
              max = 1000, step = 100, width = "100%"
            )
          ),
          column(
            5,
            sliderInput(
              inputId = "segment_entropy_minkb",
              label = "Minimum segment size (in kb) for a segment",
              value = 1000,
              min = 500,
              max = 10000, step = 500, width = "100%"
            )
          ),
          column(
            3,
            sliderInput(
              inputId = "segment_entropy_nbin",
              label = "Number of bins",
              value = 10,
              min = 10,
              max = 100, step = 5, width = "100%"
            )
          )

        ),

        # 7.EstimateClonality
        # No parameters

        # 8. Shannon entropy (RNA expression)
        conditionalPanel(
          condition = 'input.ith_algorithm == "expressionEntropy"',
          column(
            3,
            radioSwitchButtons(
              inputId = "expression_entropy_normalized",
              label = "Adjusted by features number?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            )
          )

        ),
        # 9. Diversity_Multiregional_scRNAseq
        conditionalPanel(
          condition = 'input.ith_algorithm == "Diversity_Multiregional_scRNAseq"',
          column(
            2,
            radioSwitchButtons(
              inputId = "is_scrnaseq",
              label = "scRNAseq?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            )
          ),
          column(
            4,
            radioSwitchButtons(
              inputId = "pca_expression_toppc",
              label = "Selected top PC?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            ),
            fluidRow(
              conditionalPanel(
                condition = 'input.pca_expression_toppc == "yes"',
                column(12,
                       style = "height:60px;",
                       sliderInput(
                         inputId = "pca_expression_pc_number",
                         label = "Number of PC",
                         value = 10,
                         min = 5,
                         max = 50, step = 1, width = "100%"
                       )
                )
              )
            )
          ),
          column(
            6,
            radioSwitchButtons(
              inputId = "pca_expression_rm_outlier",
              label = "Remove the extreme PC value",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            ),
            fluidRow(
              conditionalPanel(
                condition = 'input.pca_expression_rm_outlier == "yes"',
                column(
                  3,
                  style = "height:50px;",
                  numericInput(
                    inputId = "pca_expression_sd_times",
                    label = tags$span(
                      add_prompt(tags$span(icon(name = "circle-question")),
                                 message = "Enter the times of SD want to be used, deafult the max is 3.",
                                 position = "right"
                      ),
                      "SD times"
                    ),
                    value = 3,
                    min = 1,
                    max = 3, step = 1, width = "100%"
                  )
                ),
                column(
                  3,
                  numericInput(
                    inputId = "sample_drop_percent",
                    label = tags$span(
                      add_prompt(tags$span(icon(name = "circle-question")),
                                 message = "Default is 50%",
                                 position = "right"
                      ),
                      "Percent cutoff"
                    ),
                    value = 0.5,
                    min = 0.1,
                    max = 0.9, step = 0.1, width = "100%"
                  )
                )
              )
            )
          )
        ),
        # 10. SD
        # No parameters

        # 11. sMPD_Multiregional
        conditionalPanel(
          condition = 'input.ith_algorithm == "sMPD_Multiregional"',
          column(
            5,
            radioSwitchButtons(
              inputId = "distance_expression_toppc",
              label = "Selected top PC?",
              choices = c("No" = "no", "Yes" = "yes"),
              selected_background = "#00A064"
            ),
            fluidRow(
              conditionalPanel(
                condition = 'input.distance_expression_toppc == "yes"',
                column(12,
                       style = "height:60px;",
                       sliderInput(
                         inputId = "distance_expression_pc_number",
                         label = "Number of PC",
                         value = 10,
                         min = 5,
                         max = 50, step = 1, width = "100%"
                       )
                )
              )
            )
          ),
          column(
            3,
            selectInput(inputId = 'distance_expression_method',
                        label = "Method used fo measure distance",
                        choices = c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski"),
                        selected = 'euclidean',
                        multiple = F)
          )
        ),

        # 12. methylationEntropy
        # No parameters
      )
    )
  ),
  # STEP3 ----
  fluidRow(
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE,
      solidHeader = TRUE,
      title = strong("STEP3, Upload your data file!",
                     style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;", # display:block;padding:10px;position:relative;
      fluidRow(
      column(6,
             style = "margin-left:-15px",
             # 1. VAF_CCF
             conditionalPanel(
               condition = 'input.ith_data_type == "VAF_CCF"',
               conditionalPanel(
                 # 1.1 jaccardSimilarity
                 condition = 'input.ith_algorithm == "jaccardSimilarity"',
                 fileInput(
                   inputId = "file_mutation_jaccard",
                   label = HTML(
                     "Upload multiregional mutation file (",
                     as.character(actionLink(
                       inputId = "demo_jaccard_mutation",
                       label = " example",
                       icon = icon("file")
                     )),
                     ")"

                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
                ),
               conditionalPanel(
                 # 1.2 MAD
                 condition = 'input.ith_algorithm == "MAD"',
                 fileInput(
                   inputId = "file_mutation_mad",
                   label = HTML(
                     "Upload mutation file (",
                     as.character(actionLink(
                       inputId = "demo_mad_mutation",
                       label = " example",
                       icon = icon("file")
                     )),
                     ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 1.3 Shannon entropy
                 condition = 'input.ith_algorithm == "mutationEntropy"',
                 fileInput(
                   inputId = "file_mutation_entropy",
                   label = HTML(
                     "Upload mutation file (",
                     as.character(actionLink(
                       inputId = "demo_entropy_mutation",
                       label = " example[VAF]",
                       icon = icon("file")
                     )),
                     ", ",
                     as.character(actionLink(
                       inputId = "demo_entropy_mutation_ccf",
                       label = " example[CCF]",
                       icon = icon("file")
                     )),
                     ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 1.4 Ratio
                 condition = 'input.ith_algorithm == "Ratio"',
                 fileInput(
                   inputId = "file_mutation_ratio",
                   label = HTML(
                     "Upload mutation file (",
                     as.character(actionLink(
                       inputId = "demo_ccf_mutation",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               )
             ),

             # 2. Segment
             conditionalPanel(
               condition = 'input.ith_data_type == "Segment"',
               conditionalPanel(
                 # 2.1 segmentCNH
                 condition = 'input.ith_algorithm == "segmentCNH"',
                 fileInput(
                   inputId = "file_segment_distance",
                   label = HTML(
                     "Upload segment file (",
                     as.character(actionLink(
                       inputId = "demo_segment",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 ),
                 fileInput(
                   inputId = "file_purity_ploidy",
                   label = HTML(
                     "Upload purity and ploidy file (optional)(",
                     as.character(actionLink(
                       inputId = "demo_purity_ploidy",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv")
                 )
               ),
               conditionalPanel(
                 # 2.2 Shannon entropy
                 condition = 'input.ith_algorithm == "segmentEntropy"',
                 fileInput(
                   inputId = "file_segment_entropy",
                   label = HTML(
                     "Upload segment file (",
                     as.character(actionLink(
                       inputId = "demo_segment_logr_entropy",
                       label = " example[BAF/LRR]",
                       icon = icon("file")
                     )),
                     ", ",
                     as.character(actionLink(
                       inputId = "demo_segment_cbs_entropy",
                       label = " example[CBS]",
                       icon = icon("file")
                     )),
                     ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 2.3 EstimateClonality
                 condition = 'input.ith_algorithm == "EstimateClonality"',
                 fileInput(
                   inputId = "file_ec_segment",
                   label = HTML(
                     "Upload segment file (",
                     as.character(actionLink(
                       inputId = "demo_ec_segment",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 ),
                 fileInput(
                   inputId = "file_ec_mutation",
                   label = HTML(
                     "Upload mutation file (",
                     as.character(actionLink(
                       inputId = "demo_ec_mutation",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv")
                 )
               )

             ),
             # 3. Expression
             conditionalPanel(
               condition = 'input.ith_data_type == "Expression"',
               conditionalPanel(
                 # 3.1 expressionEntropy
                 condition = 'input.ith_algorithm == "expressionEntropy"',
                 fileInput(
                   inputId = "file_expression_entropy",
                   label = HTML(
                     "Upload normalized (e.g. TPM) expression file (",
                     as.character(actionLink(
                       inputId = "demo_expression_entropy",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 3.2 Diversity_Multiregional_scRNAseq
                 condition = 'input.ith_algorithm == "Diversity_Multiregional_scRNAseq"',
                 fileInput(
                   inputId = "file_expression_pca",
                   label = HTML(
                     "Upload multiregional or single-cell normalized expression file (",
                     as.character(actionLink(
                       inputId = "demo_expression_pca",
                       label = " example[Multiregional]",
                       icon = icon("file")
                     )),
                     ", ",
                     as.character(actionLink(
                       inputId = "demo_expression_pca_scrnaseq",
                       label = " example[scRNAseq]",
                       icon = icon("file")
                     )),
                     ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 ),

                 fileInput(
                   inputId = "file_meta_pca",
                   label = HTML(
                     "Upload meta file of multiregional or single-cell sequencing samples (",
                     as.character(actionLink(
                       inputId = "demo_meta_pca",
                       label = " example[Multiregional]",
                       icon = icon("file")
                     )),
                     ", ",
                     as.character(actionLink(
                       inputId = "demo_meta_pca_scrnaseq",
                       label = " example[scRNAseq]",
                       icon = icon("file")
                     )),
                     ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 ),
                 conditionalPanel(
                   condition = 'input.is_scrnaseq == "no"',
                   fileInput(
                     inputId = "file_coordinate_pca",
                     label = HTML(
                       "Upload coordinate file of multiregional sequencing samples (optional) (",
                       as.character(actionLink(
                         inputId = "demo_coordinate_pca",
                         label = " example",
                         icon = icon("file")
                       )), ")"
                     ),
                     multiple = FALSE,
                     width = "100%",
                     accept = c(".txt", ".tsv", ".csv"),
                   )
                 )
               ),
               conditionalPanel(
                 # 3.3 SD
                 condition = 'input.ith_algorithm == "SD"',
                 fileInput(
                   inputId = "file_expression_sd",
                   label = HTML(
                     "Upload expression file of bulk sequencing samples (",
                     as.character(actionLink(
                       inputId = "demo_expression_sd",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 3.4 sMPD_Multiregional
                 condition = 'input.ith_algorithm == "sMPD_Multiregional"',
                 fileInput(
                   inputId = "file_expression_distance",
                   label = HTML(
                     "Upload expression file of multiregional sequencing samples (",
                     as.character(actionLink(
                       inputId = "demo_expression_distance",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 ),
                 fileInput(
                   inputId = "file_expression_distance_meta",
                   label = HTML(
                     "Upload meta file of expression data (",
                     as.character(actionLink(
                       inputId = "demo_expression_distance_meta",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               )
             ),
             # 4. Protein (合并到RNA expression了)
             conditionalPanel(
               condition = 'input.ith_data_type == "Protein"',
               conditionalPanel(
                 # 4.1 expressionEntropy
                 condition = 'input.ith_algorithm == "expressionEntropy"',
                 fileInput(
                   inputId = "file_protein_entropy",
                   label = HTML(
                     "Upload normalized protein abundance file (",
                     as.character(actionLink(
                       inputId = "demo_expression_entropy", # demo_expression_entropy. demo_protein_entropy
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 4.2 sMPD_Multiregional
                 condition = 'input.ith_algorithm == "sMPD_Multiregional"',
                 fileInput(
                   inputId = "file_protein_distance",
                   label = HTML(
                     "Upload normalized protein abundance file (",
                     as.character(actionLink(
                       inputId = "demo_protein_distance",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               )
             ),
             # 5. Methylation
             conditionalPanel(
               condition = 'input.ith_data_type == "Methylation"',
               conditionalPanel(
                 # 5.1 methylationEntropy
                 condition = 'input.ith_algorithm == "methylationEntropy"',
                 fileInput(
                   inputId = "file_methylation_entropy",
                   label = HTML(
                     "Upload methylation file (",
                     as.character(actionLink(
                       inputId = "demo_methylation_entropy",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               ),
               conditionalPanel(
                 # 5.2 SD
                 condition = 'input.ith_algorithm == "SD"',
                 fileInput(
                   inputId = "file_methylation_sd",
                   label = HTML(
                     "Upload methylation file (",
                     as.character(actionLink(
                       inputId = "demo_methylation_sd",
                       label = " example",
                       icon = icon("file")
                     )), ")"
                   ),
                   multiple = FALSE,
                   width = "100%",
                   accept = c(".txt", ".tsv", ".csv"),
                 )
               )
             )
      ),
      column(
        6,
        fluidRow( # submit
          style = "margin-top:24.5px", # 与第一个输入框对齐
          actionButton(
            inputId = "run_analysis",
            label = "RUN!",
            class = "btn-success"
          ),
          # shinyalert::useShinyalert(),
          actionButton(
            inputId = "run_reset",
            label = "RESET",
            class = "btn-warning"
          ),
          actionButton(
            inputId = "run_demo",
            label = "DEMO",
            class = "btn-primary"
          )
        )
      )),
      fluidRow(
        column(
          12,
          # style = "margin-top:20px;",
          box(uiOutput(outputId = "ui_input_data"),
              collapsible = TRUE, collapsed = FALSE,
              solidHeader = TRUE,
              title = strong("View the upload data!",
                             style = "font-size:20px;color:white;"
              ),
              background = "black",
              style = "background-color:#ffffff;color:black;",
              # status = "success",
              width = 12,
              tags$p(strong("Note: "),"Only the first 10 rows and 20 columns of uploaded data are displayed!")
          )
        )
      )
    )
  ),

  # STEP4 ----
  fluidRow(
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE,
      solidHeader = TRUE,
      title = strong("STEP4, View the ITH index!",
                     style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;",
      fluidRow(
        column(6,
               style = "margin-top:20px;",
               uiOutput(outputId = "ui_ith_output_table")),
        column(6,
               style = "margin-top:20px;",
               uiOutput(outputId = "ui_ith_output_graphic")
        )
      )
      # fluidRow(
        # column(6, fluidRow(uiOutput(outputId = "dl_ui_ith_output_table")))
        # column(6, fluidRow(uiOutput(outputId = "dl_ui_ith_output_graphic"),style = "text-align:right !important;"))
      # )
    )
  ),

  # show example data ----------------------------------------------------------
  # 1. Mutation (vAF/CCF) ----
  bsModal(
    id = "readme1_1", title = strong("Prepare input Data"), trigger = "demo_jaccard_mutation", size = "large",
    tags$p("To calculate the Jaccard Similarity with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of multi-regional samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",tags$a(strong("PatientID, SampleID and Hugo_Symbol."),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),
    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_jaccard_mutation")
  ),
  bsModal(
    id = "readme1_2", title = strong("Prepare input Data"), trigger = "demo_mad_mutation", size = "large",
    tags$p("To calculate the MATH score with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Tumor_VAF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is FALSE) or ",style = "color:#000"),
            tags$a(strong("CCF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is TRUE)",style = "color:#000")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),
    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_mad_mutation")
  ),
  bsModal(
    id = "readme1_3_1", title = strong("Prepare input Data"), trigger = "demo_entropy_mutation", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Tumor_VAF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is FALSE) or ",style = "color:#000"),
            tags$a(strong("CCF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is TRUE)",style = "color:#000")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_entropy_mutation")
  ),
  bsModal(
    id = "readme1_3_2", title = strong("Prepare input Data"), trigger = "demo_entropy_mutation_ccf", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Tumor_VAF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is FALSE) or ",style = "color:#000"),
            tags$a(strong("CCF"),style = "color:#000; font-size:16px;font-family: sans-serif"),
            tags$a("(if the Cancer Cell Fraction is TRUE)",style = "color:#000")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_entropy_mutation_ccf")
  ),
  bsModal(
    id = "readme1_4", title = strong("Prepare input Data"), trigger = "demo_ccf_mutation", size = "large",
    tags$p("To calculate the Ratio with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID")," and ",strong("CCF"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_ccf_mutation")
  ),
  # 2. Copy Number Variant ----
  bsModal(
    id = "readme2_1_1", title = strong("Prepare input Data"), trigger = "demo_segment", size = "large",
    tags$p("To calculate the Copy number heterogeneity (CNHplus) with ITHindex, you need to provide:"),
    tags$li("The Relative Copy Number (RCN) profilefile of a single-region samples from patients, can be obtained either by the microarray technology or
            by shallow Whole Genome Sequencing.",strong("Required")),
    tags$li("The RCN profile data frame should comprise the following variables, at minimum: ",
            tags$a(strong("SampleID, Start, End, Segment_Mean"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("Note that CNHplus assumes that Segment_Mean is the relative copy number of the segment, not its log2 transformation."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example segment data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_segment")
  ),
  bsModal(
    id = "readme2_1_2", title = strong("Prepare input Data"), trigger = "demo_purity_ploidy", size = "large",
    tags$p("To calculate the Copy number heterogeneity with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Purity, Ploidy"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$li("SampleID should be consistent in all input files, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example Purity and Ploidy data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_purity_ploidy")
  ),
  bsModal(
    id = "readme2_2_1", title = strong("Prepare input Data"), trigger = "demo_segment_logr_entropy", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Chr, Start, End, nProbes, mBAF, logR"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example segment logR data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_segment_logr_entropy")
  ),
  bsModal(
    id = "readme2_2_2", title = strong("Prepare input Data"), trigger = "demo_segment_cbs_entropy", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Chr, Start, End, nProbes, Segment_Mean"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example segment CBS data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_segment_cbs_entropy")
  ),
  bsModal(
    id = "readme2_3_1", title = strong("Prepare input Data"), trigger = "demo_ec_mutation", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("Patient, Chr, Start_position, End_position, Reference, Alternate, Variant_freq, Ref_freq, Hugo_Symbol, Variant_Classification"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example mutation data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_ec_mutation")
  ),
  bsModal(
    id = "readme2_3_2", title = strong("Prepare input Data"), trigger = "demo_ec_segment", size = "large",
    tags$p("To calculate the Shannon entropy with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("SampleID, Chr, Start, End, nProbes, cn"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each sample should be unique."),
    tags$li("SampleID should be consistent with Patient in mutation file, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example segment data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_ec_segment")
  ),
  # 3. RNA expression ----
  bsModal(
    id = "readme3_1", title = strong("Prepare input Data"), trigger = "demo_expression_entropy", size = "large",
    tags$p("To calculate the Shannon entropy using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("gene_id"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The gene_id of each row should be unique."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example expression data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_expression_entropy")
  ),
  bsModal(
    id = "readme3_2_1_1", title = strong("Prepare input Data"), trigger = "demo_expression_pca", size = "large",
    tags$p("To calculate the diversity score using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of multi-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("Hugo_symbol"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The Hugo_symbol of each row should be unique."),
    tags$li("The colnames should be consistent with SampleID in meta or coordinate file."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example expression data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_expression_pca")
  ),
  bsModal(
    id = "readme3_2_1_2", title = strong("Prepare input Data"), trigger = "demo_expression_pca_scrnaseq", size = "large",
    tags$p("To calculate the diversity score using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-cell samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("Hugo_symbol"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The Hugo_symbol of each row should be unique."),
    tags$li("The colnames should be consistent with SampleID in meta file."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example expression data (scRNAseq)"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_expression_pca_scrnaseq")
  ),
  bsModal(
    id = "readme3_2_2_1", title = strong("Prepare input Data"), trigger = "demo_meta_pca", size = "large",
    tags$p("To calculate the diversity score using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of multi-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("PatientID, SampleID"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each row should be unique."),
    tags$li("SampleID should be consistent in all input files, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example meta data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_meta_pca")
  ),
  bsModal(
    id = "readme3_2_2_2", title = strong("Prepare input Data"), trigger = "demo_meta_pca_scrnaseq", size = "large",
    tags$p("To calculate the diversity score using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of single-cell samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("PatientID, SampleID"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each row should be unique."),
    tags$li("SampleID should be consistent in all input files, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example meta data (scRNAseq)"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_meta_pca_scrnaseq")
  ),
  bsModal(
    id = "readme3_2_3", title = strong("Prepare input Data"), trigger = "demo_coordinate_pca", size = "large",
    tags$p("To calculate the diversity score using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of multi-region samples from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("PatientID, SampleID, x, y"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each row should be unique."),
    tags$li("SampleID should be consistent in all input files, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example coordinate data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_coordinate_pca")
  ),
  bsModal(
    id = "readme3_3", title = strong("Prepare input Data"), trigger = "demo_expression_sd", size = "large",
    tags$p("To calculate the SD using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of mRNA expression from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("gene_id"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example expression data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_expression_sd")
  ),
  bsModal(
    id = "readme3_4_1", title = strong("Prepare input Data"), trigger = "demo_expression_distance", size = "large",
    tags$p("To calculate the scaled mean pairwise distance (sMPD) using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of mRNA expression from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("gene_id"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example expression data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_expression_distance")
  ),
  bsModal(
    id = "readme3_4_2", title = strong("Prepare input Data"), trigger = "demo_expression_distance_meta", size = "large",
    tags$p("To calculate the scaled mean pairwise distance (sMPD) using RNA expression or protein abundance data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of mRNA expression from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("PatientID, SampleID"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$li("The SampleID of each row should be unique."),
    tags$li("SampleID should be consistent in all input files, respectively."),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example meta data"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_meta_distance")
  ),
  # 5. Methylation ----
  bsModal(
    id = "readme5_1", title = strong("Prepare input Data"), trigger = "demo_methylation_entropy", size = "large",
    tags$p("To calculate the entropy using methyaltion data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of methyaltion matrix from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("gene_id"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example: methyaltion matrix"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_methylation_entropy")
  ),
  bsModal(
    id = "readme5_2", title = strong("Prepare input Data"), trigger = "demo_methylation_sd", size = "large",
    tags$p("To calculate the SD using methyaltion data with ITHindex, you need to provide:"),
    tags$li("A tab-delimited text file of methyaltion matrix from patients.",strong("Required")),
    tags$li("The mandatory fields: ",
            tags$a(strong("gene_id"),style = "color:#000; font-size:16px;font-family: sans-serif")),
    tags$p(strong('Note: '),'If adjustments are made to the parameters, it is necessary to resubmit again.'),

    tags$hr(),
    tags$h4(strong("Example: methyaltion matrix"), style = "color:#00bf78; font-family: sans-serif"),
    DTOutput("example_methylation_sd")
  )

  # show example data  end -----------------------------------------------------
)

ith_quantification_page$ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE, minified = FALSE),
  quantification_page
)


ith_quantification_page$server <- function(input, output, session) {
  # update AwesomeRadio
  observe({
    selected_data_type <- input$ith_data_type
    message("user selected data types: ", selected_data_type)
    if (selected_data_type %in% c("VAF_CCF")) choices <- c("Jaccard Similarity [Multiregional]" = "jaccardSimilarity",
                                                           "MATH" = "MAD",
                                                           "Proportion of clonality (pClonality)" = "Ratio",
                                                           "Shannon entropy" = "mutationEntropy")
    if (selected_data_type %in% c("Segment")) choices <- c("Proportion of late-stage mutations (pLM)" = "EstimateClonality",
                                                           "CNHplus" = "segmentCNH",
                                                           "Shannon entropy" = "segmentEntropy"
                                                           # ,"Ripley’s K" = "RipleysK"
                                                           )
    if (selected_data_type %in% c("Expression")) choices <- c("Standard deviations"="SD",
                                                              "Shannon entropy" = "expressionEntropy",
                                                              "Scaled mean pairwise distance (sMPD) [Multiregional]"="sMPD_Multiregional",
                                                              "Diversity score [Multiregional or scRNAseq]"="Diversity_Multiregional_scRNAseq")
    if (selected_data_type %in% c("Methylation")) choices <- c("Standard deviations" = "SD",
                                                               "Shannon entropy" = "methylationEntropy")
    updateAwesomeRadio(
      session = session,
      inputId = "ith_algorithm",
      choices = c(choices),
      selected = choices[1],
      inline = T,
      status = "success",
      checkbox = TRUE
    )
  })

  # Check Input ----------------------------------------------------------------
  # check the input data format
  iv_input_check <- InputValidator$new()
  iv_input_check$add_rule("file_mutation_jaccard", sv_required())
  iv_input_check$add_rule("file_mutation_mad", sv_required())
  iv_input_check$add_rule("file_mutation_entropy", sv_required())
  iv_input_check$add_rule("file_mutation_ratio", sv_required())

  iv_input_check$add_rule("file_segment_distance", sv_required())
  # iv_input_check$add_rule("file_purity_ploidy", sv_required())
  iv_input_check$add_rule("file_segment_entropy", sv_required())
  iv_input_check$add_rule("file_ec_segment", sv_required())
  iv_input_check$add_rule("file_ec_mutation", sv_required())

  iv_input_check$add_rule("file_expression_entropy", sv_required())
  iv_input_check$add_rule("file_expression_pca", sv_required())
  iv_input_check$add_rule("file_meta_pca", sv_required())
  # iv_input_check$add_rule("file_coordinate_pca", sv_required())
  iv_input_check$add_rule("file_expression_sd", sv_required())
  iv_input_check$add_rule("file_expression_distance", sv_required())
  iv_input_check$add_rule("file_expression_distance_meta", sv_required())

  iv_input_check$add_rule("file_methylation_entropy", sv_required())
  iv_input_check$add_rule("file_methylation_sd", sv_required())

  iv_input_check$add_rule(
    "entropy_weighted_value",
    sv_numeric(
      message = "Numbers are required",
      allow_multiple = TRUE
    )
  )
  iv_input_check$enable()

  # disable the btn if no file upload
  # observe({
  #   shinyjs::toggleState("run_analysis")
  # })

  # show example data ----------------------------------------------------------
  observe({
    input_ith_data_type <- reactive({
      selected_data_type <- input$ith_data_type
      # message("user selected data types: ",selected_data_type)
      return(selected_data_type)
    })
    input_ith_algorithm <- reactive({
      selected_algorithm <- input$ith_algorithm
      # message("user selected algorithm: ",selected_algorithm)
      return(selected_algorithm)
    })
    # 1. Mutation (VAF/CCF) ----
    if (input_ith_data_type() == "VAF_CCF") {

      if(input_ith_algorithm() == 'jaccardSimilarity'){
        example_file <- reactive({
          data <- data.table::fread(file = demo_file_mutation_multiregional)
          return(data)
        })
      }
      if(input_ith_algorithm() %in% c('MAD','mutationEntropy')){
        example_file <- reactive({
          data <- data.table::fread(file = demo_file_mutation_singleregional)
          return(data)
        })
      }
      observeEvent(input$demo_jaccard_mutation, {
        output$example_jaccard_mutation <- renderDT(example_file(),
                                                    rownames = FALSE,
                                                    options = list( # dom = 't',
                                                      pageLength = 20,
                                                      scrollY = "300px",
                                                      scrollX = TRUE,
                                                      columnDefs = list(list(className = 'dt-center',
                                                                             targets = "_all")
                                                      )
                                                    )
        )
      })
      observeEvent(input$demo_mad_mutation, {
        output$example_mad_mutation <- renderDT(example_file(),
                                                rownames = FALSE,
                                                options = list( # dom = 't',
                                                  pageLength = 20,
                                                  scrollY = "300px",
                                                  scrollX = TRUE,
                                                  columnDefs = list(list(className = 'dt-center',
                                                                         targets = "_all")
                                                  )
                                                )
        )
      })
      observeEvent(input$demo_entropy_mutation, {
        output$example_entropy_mutation <- renderDT(example_file(),
                                                    rownames = FALSE,
                                                    options = list( # dom = 't',
                                                      pageLength = 20,
                                                      scrollY = "300px",
                                                      scrollX = TRUE,
                                                      columnDefs = list(list(className = 'dt-center',
                                                                             targets = "_all")
                                                      )
                                                    )
        )
      })
      observeEvent(input$demo_entropy_mutation_ccf, {
        example_file <- reactive({
          data <- read.csv(file = demo_file_ccf)
          return(data)
        })
        output$example_entropy_mutation_ccf <- renderDT(example_file(),
                                                        rownames = FALSE,
                                                        options = list( # dom = 't',
                                                          pageLength = 20,
                                                          scrollY = "300px",
                                                          scrollX = TRUE,
                                                          columnDefs = list(list(className = 'dt-center',
                                                                                 targets = "_all")
                                                          )
                                                        )
        )
      })
      observeEvent(input$demo_ccf_mutation, {
        example_file <- reactive({
          data <- read.csv(file = demo_file_ccf)
          return(data)
        })
        output$example_ccf_mutation <- renderDT(example_file(),
                                                rownames = FALSE,
                                                options = list( # dom = 't',
                                                  pageLength = 20,
                                                  scrollY = "300px",
                                                  scrollX = TRUE,
                                                  columnDefs = list(list(className = 'dt-center',
                                                                         targets = "_all")
                                                  )
                                                )
        )
      })
    }
    # 2. Copy number variant ----
    if (input_ith_data_type() == "Segment") {
      if (input_ith_algorithm() == "segmentCNH") {
        example_file_seg_cnh <- reactive({
          data <- data.table::fread(file = demo_file_segment)
          return(data)
        })
        example_file_purity_ploidy <- reactive({
          data <- data.table::fread(file = demo_file_purity_ploidy)
          return(data)
        })
      }
      if (input_ith_algorithm() == "segmentEntropy") {
        example_file_seg_div <- reactive({
          data <- data.table::fread(file = demo_file_segment_logR)
          return(data)
        })
        example_file_seg_cbs <- reactive({
          data <- data.table::fread(file = demo_file_segment)
          return(data)
        })
      }
      if (input_ith_algorithm() == "EstimateClonality") {
        example_file_seg <- reactive({
          data <- data.table::fread(file = demo_file_segment_nAnB)
          return(data)
        })
        example_file_mut <- reactive({
          data <- data.table::fread(file = demo_file_ec_mutation)
          return(data)
        })
      }
      observeEvent(input$demo_segment, {
        output$example_segment <- renderDT(example_file_seg_cnh(),
                                           rownames = FALSE,
                                           options = list( # dom = 't',
                                             pageLength = 20,
                                             scrollY = "300px",
                                             scrollX = TRUE
                                           )
        )
      })
      observeEvent(input$demo_purity_ploidy, {
        output$example_purity_ploidy <- renderDT(example_file_purity_ploidy(),
                                                 rownames = FALSE,
                                                 options = list( # dom = 't',
                                                   pageLength = 20,
                                                   scrollY = "300px",
                                                   scrollX = TRUE
                                                 )
        )
      })
      observeEvent(input$demo_segment_logr_entropy, {
        output$example_segment_logr_entropy <- renderDT(example_file_seg_div(),
                                                   rownames = FALSE,
                                                   options = list( # dom = 't',
                                                     pageLength = 20,
                                                     scrollY = "300px",
                                                     scrollX = TRUE
                                                   )
        )
      })
      observeEvent(input$demo_segment_cbs_entropy, {
        output$example_segment_cbs_entropy <- renderDT(example_file_seg_cbs(),
                                                   rownames = FALSE,
                                                   options = list( # dom = 't',
                                                     pageLength = 20,
                                                     scrollY = "300px",
                                                     scrollX = TRUE
                                                   )
        )
      })


      observeEvent(input$demo_ec_mutation, {
        output$example_ec_mutation <- renderDT(example_file_mut(),
                                               rownames = FALSE,
                                               options = list( # dom = 't',
                                                 pageLength = 20,
                                                 scrollY = "300px",
                                                 scrollX = TRUE
                                               )
        )
      })
      observeEvent(input$demo_ec_segment, {
        output$example_ec_segment <- renderDT(example_file_seg(),
                                              rownames = FALSE,
                                              options = list( # dom = 't',
                                                pageLength = 20,
                                                scrollY = "300px",
                                                scrollX = TRUE
                                              )
        )
      })
    }


    # 3. RNA expression ----
    if (input_ith_data_type() == "Expression") {
      if (input_ith_algorithm() == "expressionEntropy") {
        example_file_expr <- reactive({
          data <- data.table::fread(file = demo_file_bulkRNAseq_TPM)
          return(data)
        })

      }
      if (input_ith_algorithm() == "Diversity_Multiregional_scRNAseq") {
        example_file_expr_multibulkrnaseq <- reactive({
          data <- data.table::fread(file = demo_file_bulkRNAseq_TPM_multiregional)
          return(data)
        })
        example_file_meta_multibulkrnaseq <- reactive({
          data <- data.table::fread(file = demo_file_bulkRNAseq_coordinate_multiregional)
          data %<>%
            dplyr::select(PatientID,SampleID)
          return(data)
        })
        example_file_coordinate_multibulkrnaseq <- reactive({
          data <- data.table::fread(file = demo_file_bulkRNAseq_coordinate_multiregional)
          return(data)
        })

        example_file_expr_scrnaseq <- reactive({
          data <- data.table::fread(file = demo_file_scRNAseq_TPM)
          return(data)
        })
        example_file_meta_scrnaseq <- reactive({
          data <- data.table::fread(file = demo_file_scRNAseq_meta)
          return(data)
        })

      }
      if (input_ith_algorithm() == "SD") {
        example_file_sd <- reactive({
          data <- data.table::fread(file = demo_file_bulkRNAseq_TPM)
          return(data)
        })

      }
      if (input_ith_algorithm() == "sMPD_Multiregional") {
        example_file_distance <- reactive({
          data <- data.table::fread(file = demo_file_proteome_multiregional)
          return(data)
        })
        example_file_distance_meta <- reactive({
          data <- data.table::fread(file = demo_file_proteome_meta_multiregional)
          return(data)
        })

      }

      observeEvent(input$demo_expression_entropy, {
        output$example_expression_entropy <- renderDT(example_file_expr(),
                                           rownames = FALSE,
                                           options = list( # dom = 't',
                                             pageLength = 20,
                                             scrollY = "300px",
                                             scrollX = TRUE
                                           )
        )
      })
      observeEvent(input$demo_expression_pca, {
        output$example_expression_pca <- renderDT(example_file_expr_multibulkrnaseq(),
                                                      rownames = FALSE,
                                                      options = list( # dom = 't',
                                                        pageLength = 20,
                                                        scrollY = "300px",
                                                        scrollX = TRUE
                                                      )
        )
      })
      observeEvent(input$demo_meta_pca, {
        output$example_meta_pca <- renderDT(example_file_meta_multibulkrnaseq(),
                                                  rownames = FALSE,
                                                  options = list( # dom = 't',
                                                    pageLength = 20,
                                                    scrollY = "300px",
                                                    scrollX = TRUE
                                                  )
        )
      })
      observeEvent(input$demo_coordinate_pca, {
        output$example_coordinate_pca <- renderDT(example_file_coordinate_multibulkrnaseq(),
                                            rownames = FALSE,
                                            options = list( # dom = 't',
                                              pageLength = 20,
                                              scrollY = "300px",
                                              scrollX = TRUE
                                            )
        )
      })
      observeEvent(input$demo_expression_pca_scrnaseq, {
        output$example_expression_pca_scrnaseq <- renderDT(example_file_expr_scrnaseq(),
                                                  rownames = FALSE,
                                                  options = list( # dom = 't',
                                                    pageLength = 20,
                                                    scrollY = "300px",
                                                    scrollX = TRUE
                                                  )
        )
      })
      observeEvent(input$demo_meta_pca_scrnaseq, {
        output$example_meta_pca_scrnaseq <- renderDT(example_file_meta_scrnaseq(),
                                            rownames = FALSE,
                                            options = list( # dom = 't',
                                              pageLength = 20,
                                              scrollY = "300px",
                                              scrollX = TRUE
                                            )
        )
      })
      observeEvent(input$demo_expression_sd, {
        output$example_expression_sd <- renderDT(example_file_sd(),
                                                      rownames = FALSE,
                                                      options = list( # dom = 't',
                                                        pageLength = 20,
                                                        scrollY = "300px",
                                                        scrollX = TRUE
                                                      )
        )
      })
      observeEvent(input$demo_expression_distance, {
        output$example_expression_distance <- renderDT(example_file_distance(),
                                                 rownames = FALSE,
                                                 options = list( # dom = 't',
                                                   pageLength = 20,
                                                   scrollY = "300px",
                                                   scrollX = TRUE
                                                 )
        )
      })
      observeEvent(input$demo_expression_distance_meta, {
        output$example_meta_distance <- renderDT(example_file_distance_meta(),
                                                       rownames = FALSE,
                                                       options = list( # dom = 't',
                                                         pageLength = 20,
                                                         scrollY = "300px",
                                                         scrollX = TRUE
                                                       )
        )
      })


    }

    # 4. Protein (合并到RNAexpression) ----

    # 5. Methylation ----
    if (input_ith_data_type() == "Methylation"){
      example_file <- reactive({
        data <- data.table::fread(file = demo_file_methylation)
        return(data)
      })
      observeEvent(input$demo_methylation_entropy, {
        output$example_methylation_entropy <- renderDT(example_file(),
                                                      rownames = FALSE,
                                                      options = list( # dom = 't',
                                                        pageLength = 20,
                                                        scrollY = "300px",
                                                        scrollX = TRUE
                                                      )
        )
      })
      observeEvent(input$demo_methylation_sd, {
        output$example_methylation_sd <- renderDT(example_file(),
                                                  rownames = FALSE,
                                                  options = list( # dom = 't',
                                                    pageLength = 20,
                                                    scrollY = "300px",
                                                    scrollX = TRUE
                                                  )
        )
      })

    }
  })
  # Clear input file -----------------------------------------------------------
  values <- reactiveValues(
    upload_state_mutation_jaccard = NULL,
    upload_state_mutation_mad = NULL,
    upload_state_mutation_entropy = NULL,
    upload_state_mutation_ratio = NULL,

    upload_state_segment_distance = NULL,
    upload_state_purity_ploidy = NULL,
    upload_state_segment_entropy = NULL,
    upload_state_ec_segment = NULL,
    upload_state_ec_mutation = NULL,

    upload_state_expression_entropy = NULL,
    upload_state_expression_pca = NULL,
    upload_state_meta_pca = NULL,
    upload_state_coordinate_pca = NULL,
    upload_state_expression_sd = NULL,
    upload_state_expression_distance = NULL,
    upload_state_expression_distance_meta = NULL,

    upload_state_methylation_entropy = NULL,
    upload_state_methylation_sd = NULL
  )
  # 点击重置动作时，将upload_state_xxx状态设置为"reset"
  observeEvent(input$run_reset, {
    values$upload_state_mutation_jaccard <- "reset"
    values$upload_state_mutation_mad <- "reset"
    values$upload_state_mutation_entropy <- "reset"
    values$upload_state_mutation_ratio <- "reset"

    values$upload_state_segment_distance <- "reset"
    values$upload_state_purity_ploidy <- "reset"
    values$upload_state_segment_entropy <- "reset"
    values$upload_state_ec_segment <- "reset"
    values$upload_state_ec_mutation <- "reset"

    values$upload_state_expression_entropy <- "reset"
    values$upload_state_expression_pca <- "reset"
    values$upload_state_meta_pca <- "reset"
    values$upload_state_coordinate_pca <- "reset"
    values$upload_state_expression_sd <- "reset"
    values$upload_state_expression_distance <- "reset"
    values$upload_state_expression_distance_meta <- "reset"

    values$upload_state_methylation_entropy <- "reset"
    values$upload_state_methylation_sd <- "reset"

  })
  # 点击上传动作时，将upload_state_xxx状态设置为"uploaded"
  observeEvent(input[[paste0("file_mutation_","jaccard")]], {
    values[[paste0('upload_state_mutation_','jaccard')]] <- "uploaded"
  })
  # observeEvent(input$file_mutation_jaccard, {
  #   values$upload_state_mutation_jaccard <- "uploaded"
  # })
  observeEvent(input$file_mutation_mad, {
    values$upload_state_mutation_mad <- "uploaded"
  })
  observeEvent(input$file_mutation_entropy, {
    values$upload_state_mutation_entropy <- "uploaded"
  })
  observeEvent(input$file_mutation_ratio, {
    values$upload_state_mutation_ratio <- "uploaded"
  })

  observeEvent(input$file_segment_distance, {
    values$upload_state_segment_distance <- "uploaded"
  })
  observeEvent(input$file_purity_ploidy, {
    values$upload_state_purity_ploidy <- "uploaded"
  })
  observeEvent(input$file_segment_entropy, {
    values$upload_state_segment_entropy <- "uploaded"
  })
  observeEvent(input$file_ec_segment, {
    values$upload_state_ec_segment <- "uploaded"
  })
  observeEvent(input$file_ec_mutation, {
    values$upload_state_ec_mutation <- "uploaded"
  })

  observeEvent(input$file_expression_entropy, {
    values$upload_state_expression_entropy <- "uploaded"
  })
  observeEvent(input$file_expression_pca, {
    values$upload_state_expression_pca <- "uploaded"
  })
  observeEvent(input$file_meta_pca, {
    values$upload_state_meta_pca <- "uploaded"
  })
  observeEvent(input$file_coordinate_pca, {
    values$upload_state_coordinate_pca <- "uploaded"
  })
  observeEvent(input$file_expression_sd, {
    values$upload_state_expression_sd <- "uploaded"
  })
  observeEvent(input$file_expression_distance, {
    values$upload_state_expression_distance <- "uploaded"
  })
  observeEvent(input$file_expression_distance_meta, {
    values$upload_state_expression_distance_meta <- "uploaded"
  })

  observeEvent(input$file_methylation_entropy, {
    values$upload_state_methylation_entropy <- "uploaded"
  })
  observeEvent(input$file_methylation_sd, {
    values$upload_state_methylation_sd <- "uploaded"
  })

  # reset output contents ------------------------------------------------------
  observeEvent(input$run_reset, {
    shinyjs::reset("quantification_page_id")
    output[["ui_ith_output_table"]] <- NULL
    output[["ui_ith_output_graphic"]] <- NULL
    output[["dl_ui_ith_output_table"]] <- NULL
    output[["dl_ui_ith_output_graphic"]] <- NULL
    output[["ui_input_data"]] <- NULL
  })
  observeEvent(input$ith_data_type, {
    shinyjs::reset("ith_algorithm")
    output[["ith_output_table"]] <- NULL
    # output[["dl_ith_output_table"]] <- NULL
    output[["ui_ith_output_table"]] <- NULL
    output[["ui_ith_output_graphic"]] <- NULL
    output[["dl_ui_ith_output_table"]] <- NULL
    output[["dl_ui_ith_output_graphic"]] <- NULL
    output[["ui_input_data"]] <- NULL
  })
  observeEvent(input$ith_algorithm, {
    output[["ith_output_table"]] <- NULL
    # output[["dl_ith_output_table"]] <- NULL
    output[["ui_ith_output_table"]] <- NULL
    output[["ui_ith_output_graphic"]] <- NULL
    output[["dl_ui_ith_output_table"]] <- NULL
    output[["dl_ui_ith_output_graphic"]] <- NULL
    output[["ui_input_data"]] <- NULL
  })

  # Visualize the upload data ----
  observe({
    ## 1. Muation
    ## file for Jaccard Similarity
    input_file_mutation_jaccard <- reactive({
      selected_datapath <- input$file_mutation_jaccard$datapath
      return(selected_datapath)
    })
    ## file for MATH
    input_file_mutation_mad <- reactive({
      selected_datapath <- input$file_mutation_mad$datapath
      return(selected_datapath)
    })
    ## file for Shannon entropy
    input_file_mutation_entropy <- reactive({
      selected_datapath <- input$file_mutation_entropy$datapath
      return(selected_datapath)
    })
    ## file for Ratio
    input_file_mutation_ratio <- reactive({
      selected_datapath <- input$file_mutation_ratio$datapath
      return(selected_datapath)
    })
    ## 2. CNV
    ## file for segmentCNH
    input_file_segment_distance <- reactive({
      selected_datapath <- input$file_segment_distance$datapath
      return(selected_datapath)
    })
    input_file_purity_ploidy <- reactive({
      selected_datapath <- input$file_purity_ploidy$datapath
      return(selected_datapath)
    })
    ## file for Shannon entropy
    input_file_segment_entropy <- reactive({
      selected_datapath <- input$file_segment_entropy$datapath
      return(selected_datapath)
    })
    ## file for EstimateClonality
    input_file_ec_segment <- reactive({
      selected_datapath <- input$file_ec_segment$datapath
      return(selected_datapath)
    })
    input_file_ec_mutation <- reactive({
      selected_datapath <- input$file_ec_mutation$datapath
      return(selected_datapath)
    })
    ## 3. RNA expression/Protein
    ## file for Shannon entropy
    input_file_expression_entropy <- reactive({
      selected_datapath <- input$file_expression_entropy$datapath
      return(selected_datapath)
    })
    ## file for PCA
    input_file_expression_pca <- reactive({
      selected_datapath <- input$file_expression_pca$datapath
      return(selected_datapath)
    })
    input_file_meta_pca <- reactive({
      selected_datapath <- input$file_meta_pca$datapath
      return(selected_datapath)
    })
    input_file_coordinate_pca <- reactive({
      selected_datapath <- input$file_coordinate_pca$datapath
      return(selected_datapath)
    })
    ## file for SD
    input_file_expression_sd <- reactive({
      selected_datapath <- input$file_expression_sd$datapath
      return(selected_datapath)
    })
    ## file for Distance
    input_file_expression_distance <- reactive({
      selected_datapath <- input$file_expression_distance$datapath
      return(selected_datapath)
    })
    input_file_expression_distance_meta <- reactive({
      selected_datapath <- input$file_expression_distance_meta$datapath
      return(selected_datapath)
    })
    ## 4. Methylation
    ## file for Shannon entropy
    input_file_methylation_entropy <- reactive({
      selected_datapath <- input$file_methylation_entropy$datapath
      return(selected_datapath)
    })
    ## file for SD
    input_file_methylation_sd <- reactive({
      selected_datapath <- input$file_methylation_sd$datapath
      return(selected_datapath)
    })
    list_data <- list()
    show_data <- show_data2 <- show_data3 <- data <- data2 <- data3 <- data.frame()
    name1 <- name2 <- name3 <- 'data'
    if(!is.null(input_file_mutation_jaccard())) {
      data <- data.table::fread(input_file_mutation_jaccard(), encoding = "UTF-8")
      name1 = "Mutation[Multi]"
    }
    if(!is.null(input_file_mutation_mad())) {
      data <- data.table::fread(input_file_mutation_mad(), encoding = "UTF-8")
      name1 = "Mutation"
    }
    if(!is.null(input_file_mutation_entropy())) {
      data <- data.table::fread(input_file_mutation_entropy(), encoding = "UTF-8")
      name1 = "Mutation"
    }
    if(!is.null(input_file_mutation_ratio())) {
      data <- data.table::fread(input_file_mutation_ratio(), encoding = "UTF-8")
      name1 = "Mutation"
    }

    if(!is.null(input_file_segment_distance())) {
      data <- data.table::fread(input_file_segment_distance(), encoding = "UTF-8")
      name1 = "Segment"
    }
    if(!is.null(input_file_purity_ploidy())) {
      data2 <- data.table::fread(input_file_purity_ploidy(), encoding = "UTF-8")
      name2 = "Purity/ploidy"
    }

    if(!is.null(input_file_segment_entropy())) {
      data <- data.table::fread(input_file_segment_entropy(), encoding = "UTF-8")
      name1 = "Segment"
    }

    if(!is.null(input_file_ec_mutation())) {
      data <- data.table::fread(input_file_ec_mutation(), encoding = "UTF-8")
      name1 = "Mutation"
    }
    if(!is.null(input_file_ec_segment()))  {
      data2 <- data.table::fread(input_file_ec_segment(), encoding = "UTF-8")
      name2 = "Segment"
    }

    if(!is.null(input_file_expression_entropy())) {
      data <- data.table::fread(input_file_expression_entropy(), encoding = "UTF-8")
      name1 = "bulkRNAseq"
    }

    if(!is.null(input_file_expression_pca()))  {
      data <- data.table::fread(input_file_expression_pca(), encoding = "UTF-8")
      name1 = "scRNAseq/bulkRNAseq[multi]"
    }
    if(!is.null(input_file_meta_pca())) {
      data2 <- data.table::fread(input_file_meta_pca(), encoding = "UTF-8")
      name2 = "Meta"
    }
    if(!is.null(input_file_coordinate_pca())) {
      data3 <- data.table::fread(input_file_coordinate_pca(), encoding = "UTF-8")
      name3 = "Coordinate"
    }

    if(!is.null(input_file_expression_sd())) {
      data <- data.table::fread(input_file_expression_sd(), encoding = "UTF-8")
      name1 = "bulkRNAseq/Proteome"
    }

    if(!is.null(input_file_expression_distance())) {
      data <- data.table::fread(input_file_expression_distance(), encoding = "UTF-8")
      name1 = "bulkRNAseq[multi]"
    }
    if(!is.null(input_file_expression_distance_meta())) {
      data2 <- data.table::fread(input_file_expression_distance_meta(), encoding = "UTF-8")
      name2 = "Meta"
    }

    if(!is.null(input_file_methylation_entropy())) {
      data <- data.table::fread(input_file_methylation_entropy(), encoding = "UTF-8")
      name1 = "Methylation"
    }
    if(!is.null(input_file_methylation_sd())) {
      data <- data.table::fread(input_file_methylation_sd(), encoding = "UTF-8")
      name1 = "Methylation"
    }

    if(!is.null(data)&nrow(data)!=0){
      show_data <- data
      if (nrow(data)>=10) {
        show_data <- data %>%
          as.data.frame() %>%
          dplyr::sample_n(10)
      }
      if (ncol(data)>=20) {
        show_data <- show_data[,c(1:20)]
      }
      list_data[[name1]] <- show_data
    }
    if(!is.null(data2)&nrow(data2)!=0){
      show_data2 <- data2
      if (nrow(data2)>=10) {
        show_data2 <- data2 %>%
          as.data.frame() %>%
          dplyr::sample_n(10)
      }
      list_data[[name2]] <- show_data2
    }
    if(!is.null(data3)&nrow(data3)!=0){
      show_data3 <- data3
      if (nrow(data3)>=10) {
        show_data3 <- data3 %>%
          as.data.frame() %>%
          dplyr::sample_n(10)
      }
      list_data[[name3]] <- show_data3
    }
    # Visualize the input data
    idxs <- names(list_data)
    output$ui_input_data <- renderUI({
      tabs <- lapply(idxs, function(idx) {
        tabPanel(
          title = idx,
          uiOutput(paste0(idx,"_dataidx"))
        )
      })
      do.call(tabsetPanel, tabs)
    })

    lapply(idxs, function(idx){
      output[[paste0(idx,"_dataidx")]] <- renderUI({
        shinycssloaders::withSpinner(DTOutput(paste0(idx)))
      })
      output[[paste0(idx)]] <- renderDT(
        list_data[[idx]],
        rownames = FALSE,
        options = list(
          pageLength = 20,
          scrollY = "300px",
          scrollX = TRUE
        )
      )
    })
    # output$ui_input_data_table <- renderDT(
    #   show_data,
    #   rownames = FALSE,
    #   options = list(
    #     pageLength = 20,
    #     scrollY = "300px",
    #     scrollX = TRUE
    #     )
    # )
    # output$ui_input_data <- renderUI({
    #   shinycssloaders::withSpinner(DTOutput("ui_input_data_table"))
    # })

  })

  # run demo or analysis -------------------------------------------------------
  observe({
    # 1. run demo --------------------------------------------------------------
    observeEvent(input$run_demo, {

      show_modal_progress_line(
        color = "#417AC7", # DF0101,00A064
        trail_color = "#eee",
        duration = 90,
        easing = "easeOut",
        text = "Starting computation ..."
      )
      # show_modal_progress_circle(
      #   value =  0,
      #   text = "Starting computation ...",
      #   color = "#112446",
      #   stroke_width = 4,
      #   easing = "linear",
      #   duration = 1000,
      #   trail_color = "#eee",
      #   trail_width = 1,
      #   height = "200px",
      #   session = shiny::getDefaultReactiveDomain()
      # )


      # 1.1 parameter ----
      input_ith_data_type <- reactive({
        selected_data_type <- input$ith_data_type
        return(selected_data_type)
      })
      message("demo -- user selected data types: ", input_ith_data_type())

      input_ith_algorithm <- reactive({
        selected_algorithm <- input$ith_algorithm
        return(selected_algorithm)
      })
      message("demo -- user selected algorithm: ", input_ith_algorithm())
      ## 1. Muation
      ## parameter of MATH
      input_mad_ccf <- reactive({
        selected_mad_ccf <- input$mad_ccf
        return(selected_mad_ccf)
      })
      if (input_mad_ccf() == "no")  input_mad_ccf_value <- FALSE
      if (input_mad_ccf() == "yes") input_mad_ccf_value <- TRUE

      ## parameter of Shannon entropy
      input_entropy_ccf <- reactive({
        selected_entropy_ccf <- input$entropy_ccf
        return(selected_entropy_ccf)
      })
      if (input_entropy_ccf() == "no")  input_entropy_ccf_value <- FALSE
      if (input_entropy_ccf() == "yes") input_entropy_ccf_value <- TRUE
      input_entropy_nbin <- reactive({
        selected_entropy_nbin <- input$entropy_nbin
        return(selected_entropy_nbin)
      })
      input_entropy_adjusted <- reactive({
        selected_entropy_adjusted <- input$entropy_adjusted
        return(selected_entropy_adjusted)
      })
      if (input_entropy_adjusted() == "no")  input_entropy_adjusted_value <- FALSE
      if (input_entropy_adjusted() == "yes") input_entropy_adjusted_value <- TRUE
      input_entropy_weighted <- reactive({
        selected_entropy_weighted <- input$entropy_weighted
        return(selected_entropy_weighted)
      })
      if (input_entropy_weighted() == "no")  input_entropy_weighted_value <- FALSE
      if (input_entropy_weighted() == "yes") input_entropy_weighted_value <- TRUE
      input_entropy_weighted_vector <- reactive({
        raw_weight <- input$entropy_weighted_value
        if (anyNA(raw_weight) || raw_weight == "") raw_weight <- c(rep("1",input_entropy_nbin()))
        selected_entropy_weighted_vector <- text_to_numeric(raw_weight)
        return(selected_entropy_weighted_vector)
      })

      ## parameter of Ratio
      input_ratio_cutoff <- reactive({
        selected_ratio_cutoff <- input$ratio_cutoff
        return(selected_ratio_cutoff)
      })
      ## 2. CNV
      ## parameter of Shannon entropy
      input_segment_entropy_minprobes <- reactive({
        selected_segment_entropy_minprobes <- input$segment_entropy_minprobes
        return(selected_segment_entropy_minprobes)
      })
      input_segment_entropy_minkb <- reactive({
        selected_segment_entropy_minkb <- input$segment_entropy_minkb
        return(selected_segment_entropy_minkb)
      })
      input_segment_entropy_nbin <- reactive({
        selected_segment_entropy_nbin <- input$segment_entropy_nbin
        return(selected_segment_entropy_nbin)
      })

      ## 3. RNA expression/Protein
      ## parameter of Shannon entropy
      input_expression_entropy_normalized <- reactive({
        selected_expression_entropy_normalized <- input$expression_entropy_normalized
        return(selected_expression_entropy_normalized)
      })
      if (input_expression_entropy_normalized() == "no")  input_expression_entropy_normalized_value <- FALSE
      if (input_expression_entropy_normalized() == "yes") input_expression_entropy_normalized_value <- TRUE
      ## parameter of PCA
      input_pca_is_scrnaseq <- reactive({
        selected_is_scrnaseq <- input$is_scrnaseq
        return(selected_is_scrnaseq)
      })
      if (input_pca_is_scrnaseq() == "no")  input_pca_is_scrnaseq_value <- FALSE
      if (input_pca_is_scrnaseq() == "yes") input_pca_is_scrnaseq_value <- TRUE
      input_pca_expression_toppc <- reactive({
        selected_pca_expression_toppc <- input$pca_expression_toppc
        return(selected_pca_expression_toppc)
      })
      if (input_pca_expression_toppc() == "no")  input_pca_expression_toppc_value <- FALSE
      if (input_pca_expression_toppc() == "yes") input_pca_expression_toppc_value <- TRUE
      input_pca_expression_pc_number <- reactive({
        selected_pca_expression_pc_number <- input$pca_expression_pc_number
        return(selected_pca_expression_pc_number)
      })

      selected_pca_expression_rm_outlier <- reactive({
        selected_pca_expression_rm_outlier <- input$pca_expression_rm_outlier
        return(selected_pca_expression_rm_outlier)
      })
      if (selected_pca_expression_rm_outlier() == "no")  selected_pca_expression_rm_outlier_value <- FALSE
      if (selected_pca_expression_rm_outlier() == "yes") selected_pca_expression_rm_outlier_value <- TRUE
      input_pca_expression_sd_times <- reactive({
        selected_pca_expression_sd_times <- input$pca_expression_sd_times
        return(selected_pca_expression_sd_times)
      })
      input_sample_drop_percent <- reactive({
        selected_sample_drop_percent <- input$sample_drop_percent
        return(selected_sample_drop_percent)
      })

      ## parameter of Distance
      input_distance_expression_toppc <- reactive({
        selected_distance_expression_toppc <- input$distance_expression_toppc
        return(selected_distance_expression_toppc)
      })
      if (input_distance_expression_toppc() == "no")  input_distance_expression_toppc_value <- FALSE
      if (input_distance_expression_toppc() == "yes") input_distance_expression_toppc_value <- TRUE
      input_distance_expression_pc_number <- reactive({
        selected_distance_expression_pc_number <- input$distance_expression_pc_number
        return(selected_distance_expression_pc_number)
      })
      input_distance_expression_method <- reactive({
        selected_distance_expression_method <- input$distance_expression_method
        return(selected_distance_expression_method)
      })


      # 1.2 input file ----
      ## 1. Muation
      ## file for Jaccard Similarity
      input_file_mutation_jaccard <- reactive({
        selected_datapath <- input$file_mutation_jaccard$datapath
        return(selected_datapath)
      })
      ## file for MATH
      input_file_mutation_mad <- reactive({
        selected_datapath <- input$file_mutation_mad$datapath
        return(selected_datapath)
      })
      ## file for Shannon entropy
      input_file_mutation_entropy <- reactive({
        selected_datapath <- input$file_mutation_entropy$datapath
        return(selected_datapath)
      })
      ## file for Ratio
      input_file_mutation_ratio <- reactive({
        selected_datapath <- input$file_mutation_ratio$datapath
        return(selected_datapath)
      })
      ## 2. CNV
      ## file for segmentCNH
      input_file_segment_distance <- reactive({
        selected_datapath <- input$file_segment_distance$datapath
        return(selected_datapath)
      })
      input_file_purity_ploidy <- reactive({
        selected_datapath <- input$file_purity_ploidy$datapath
        return(selected_datapath)
      })
      ## file for Shannon entropy
      input_file_segment_entropy <- reactive({
        selected_datapath <- input$file_segment_entropy$datapath
        return(selected_datapath)
      })
      ## file for EstimateClonality
      input_file_ec_segment <- reactive({
        selected_datapath <- input$file_ec_segment$datapath
        return(selected_datapath)
      })
      input_file_ec_mutation <- reactive({
        selected_datapath <- input$file_ec_mutation$datapath
        return(selected_datapath)
      })
      ## 3. RNA expression/Protein
      ## file for Shannon entropy
      input_file_expression_entropy <- reactive({
        selected_datapath <- input$file_expression_entropy$datapath
        return(selected_datapath)
      })
      ## file for PCA
      input_file_expression_pca <- reactive({
        selected_datapath <- input$file_expression_pca$datapath
        return(selected_datapath)
      })
      input_file_meta_pca <- reactive({
        selected_datapath <- input$file_meta_pca$datapath
        return(selected_datapath)
      })
      input_file_coordinate_pca <- reactive({
        selected_datapath <- input$file_coordinate_pca$datapath
        return(selected_datapath)
      })
      ## file for SD
      input_file_expression_sd <- reactive({
        selected_datapath <- input$file_expression_sd$datapath
        return(selected_datapath)
      })
      ## file for Distance
      input_file_expression_distance <- reactive({
        selected_datapath <- input$file_expression_distance$datapath
        return(selected_datapath)
      })
      input_file_expression_distance_meta <- reactive({
        selected_datapath <- input$file_expression_distance_meta$datapath
        return(selected_datapath)
      })
      ## 4. Methylation
      ## file for Shannon entropy
      input_file_methylation_entropy <- reactive({
        selected_datapath <- input$file_methylation_entropy$datapath
        return(selected_datapath)
      })
      ## file for SD
      input_file_methylation_sd <- reactive({
        selected_datapath <- input$file_methylation_sd$datapath
        return(selected_datapath)
      })

      # 1.3 ITH calculation ----
      ith_index <- data.frame()
      # 1. Mutation(VAF/CCF)
      if (input_ith_data_type() == "VAF_CCF") {
        if (input_ith_algorithm() == "jaccardSimilarity"){
          # input_file_mutation_jaccard <- "data/example/multiregional_mutation.csv"
          in_data <- data.table::fread(file = demo_file_mutation_multiregional)
          ith_index <- lpy_JaccardSimilarity(in_data)
        }
        if (input_ith_algorithm() == "MAD"){
          if(!input_mad_ccf_value){
            input_file_mutation_mad <- demo_file_mutation_singleregional
          }else{
            input_file_mutation_mad <- demo_file_ccf
          }
          in_data <- data.table::fread(file = input_file_mutation_mad)
          ith_index <- lpy_calcMATH(in_data,cancer_cell_fraction = input_mad_ccf_value)
        }
        if (input_ith_algorithm() == "mutationEntropy"){
          if(!input_entropy_ccf_value){
            input_file_mutation_entropy <- demo_file_mutation_singleregional
          }else{
            input_file_mutation_entropy <- demo_file_ccf
          }
          in_data <- data.table::fread(file = input_file_mutation_entropy)
          ith_index <- lpy_calcMutationEntropy(mutation_data = in_data,
                                              cancer_cell_fraction = input_entropy_ccf_value,
                                              based = exp(1),
                                              nbins = input_entropy_nbin(),
                                              start = 0,
                                              end = 1,
                                              adjusted = input_entropy_adjusted_value,
                                              weighted = input_entropy_weighted_value,
                                              given_weights = input_entropy_weighted_vector())
        }
        if (input_ith_algorithm() == "Ratio"){

          input_file_mutation_ratio <- demo_file_ccf
          in_data <- data.table::fread(input_file_mutation_ratio)
          ith_index <- lpy_PClonality(ccf_data = in_data,
                                           clonal_ratio_ccf  = input_ratio_cutoff())
        }
      }
      # 2. CNV
      if(input_ith_data_type() == "Segment"){
        if (input_ith_algorithm() == "segmentCNH"){

          input_file_segment_distance <- demo_file_segment
          in_data_seg <- data.table::fread(file = input_file_segment_distance) %>% as.data.frame()
          input_file_purity_ploidy <- demo_file_purity_ploidy
          in_data_pp <- data.table::fread(file = input_file_purity_ploidy) %>% as.data.frame()

          is_cnhplus <- TRUE
          ith_cnhplus <- lpy_calcCNHplus(seg_data = in_data_seg,pp_data = in_data_pp,plus = is_cnhplus)
          ith_index <- ith_cnhplus %>%
            dplyr::select(SampleID,CNHplus)

          # ith_cnh <- lpy_calcCNH(seg_data = in_data_seg,pp_data = in_data_pp)
          # ith_index <- ith_cnh %>%
          #   dplyr::select(SampleID,CNH) %>%
          #   dplyr::full_join(ith_cnhplus %>%
          #                      dplyr::select(SampleID,CNHplus),
          #                    by='SampleID') %>%
          #   dplyr::select(SampleID,CNHplus,CNH)
        }
        if (input_ith_algorithm() == "segmentEntropy"){
          input_file_segment_entropy <- demo_file_segment_logR
          in_data_seg <- data.table::fread(input_file_segment_entropy) %>% as.data.frame()
          ith_index <- ldy_snpDiv(seg_data = in_data_seg,
                                  minprobes = input_segment_entropy_minprobes(),
                                  minkb = input_segment_entropy_minkb(),
                                  nbins = input_segment_entropy_nbin())
          ith_index %<>%
            dplyr::select(SampleID,`Shannon entropy` = Shannon_index,`Ripley’s K` = Ripley_index)
        }
        if (input_ith_algorithm() == "EstimateClonality"){

          input_file_ec_mutation <- demo_file_ec_mutation
          input_file_ec_segment <- demo_file_ec_segment
          if (!file.exists(input_file_ec_segment)) {
            seg_file <- demo_file_segment_nAnB
            in_data_seg <- data.table::fread(file = seg_file) %>% as.data.frame()
            list_seg <- list("segments" = in_data_seg)
            save(list_seg, file = input_file_ec_segment)
          }
          # out_dir <- demo_out_dir
          ith_index <- calcEstimateClonality(mutation_file = input_file_ec_mutation,
                                             seg_file = input_file_ec_segment,
                                             out_dir = demo_out_dir)
          ith_index %<>%
            dplyr::select(SampleID,pLM = Late)
          file.remove(input_file_ec_segment)

        }
      }
      # 3. RNAexpression/Protein
      if(input_ith_data_type() == "Expression"){
        if (input_ith_algorithm() == "expressionEntropy"){

          input_file_expression_entropy <- demo_file_bulkRNAseq_TPM
          in_data <- data.table::fread(input_file_expression_entropy)
          in_data <- in_data %>%
            tibble::column_to_rownames(var = 'gene_id')
          ith_index <- data.frame("Diversity_score" = calcExprEntropy(in_data,
                                                                      norm = input_expression_entropy_normalized_value)) %>%
            tibble::rownames_to_column(var = 'SampleID')%>%
            dplyr::select(SampleID,`Shannon entropy` = Diversity_score)

        }
        if (input_ith_algorithm() == "Diversity_Multiregional_scRNAseq"){

          if(input_pca_is_scrnaseq_value){
            # for scRNAseq
            input_file_meta_pca <- demo_file_scRNAseq_meta
            input_file_expression_pca <- demo_file_scRNAseq_TPM
            in_data <- data.table::fread(input_file_expression_pca)
            in_meta <- data.table::fread(input_file_meta_pca)
            in_coord <- NULL
          }else{
            # for multiregional_bulkRNAseq
            input_file_expression_pca <- demo_file_bulkRNAseq_TPM_multiregional
            input_file_coordinate_pca <- demo_file_bulkRNAseq_coordinate_multiregional

            in_data <- data.table::fread(input_file_expression_pca)
            in_coord <- data.table::fread(input_file_coordinate_pca)
            in_meta <- in_coord %>%
              dplyr::select(PatientID, SampleID)
          }
          ith_index <- calcPcaDiversity(in_data,in_meta,
                                        topPC = input_pca_expression_toppc_value,
                                        nPCs = input_pca_expression_pc_number(),
                                        rmOutlier = selected_pca_expression_rm_outlier_value,
                                        sdTimes = input_pca_expression_sd_times(),
                                        sampleDropPercent = input_sample_drop_percent(),
                                        coordinate = in_coord)
        }
        if (input_ith_algorithm() == "SD"){
          input_file_expression_sd <- demo_file_bulkRNAseq_TPM
          in_data <- data.table::fread(input_file_expression_sd)
          ith_index <- calcSD(in_data,adjusted = T,imputed = T)
          ith_index %<>%
            dplyr::select(SampleID,
                          `Standard deviations` = mSD)
        }
        if (input_ith_algorithm() == "sMPD_Multiregional"){

          input_file_expression_distance <- demo_file_proteome_multiregional
          input_file_expression_distance_meta <- demo_file_proteome_meta_multiregional
          in_data <- data.table::fread(input_file_expression_distance)
          in_meta <- data.table::fread(input_file_expression_distance_meta)

          ith_index <- lpy_MPD(in_data,in_meta,
                                           pca = input_distance_expression_toppc_value,
                                           nPCs = input_distance_expression_pc_number(),
                                           method = input_distance_expression_method())
          ith_index$sMPD <- func_2zeroone(ith_index$MPD) %>% round(4)
          ith_index %<>%
            dplyr::select(PatientID,sMPD)
        }
      }
      # 4. Methylation
      if(input_ith_data_type() == "Methylation") {
        if (input_ith_algorithm() == "methylationEntropy"){
          input_file_methylation_entropy <- demo_file_methylation
          in_data <- data.table::fread(input_file_methylation_entropy)
          ith_index <- ldy_calcMethylEntropy(in_data)
        }
        if (input_ith_algorithm() == "SD"){
          input_file_methylation_sd <- demo_file_methylation
          in_data <- data.table::fread(input_file_methylation_sd)
          ith_index <- calcSD(in_data)
          ith_index %<>%
            dplyr::select(SampleID,
                          `Standard deviations` = mSD)
        }
      }

      # 1.4 Table ----
      v_list <- list("ITHindex" = ith_index)
      # output$ith_output_table <- renderDT(ith_index,
      #   rownames = FALSE,
      #   options = list( # dom = 't',
      #     pageLength = 20,
      #     scrollY = "300px",
      #     scrollX = TRUE
      #   )
      # )

      # 1.5 Density plot  ----
      p_list <- list("Graphic" = p_less)
      # more than 10 records, the density will be plot
      if (nrow(ith_index) >= 8) p_list <- densityPlot(ith_index)
      # more than 2 indexs, the corr will be plot
      # if(nrow(ith_index) >= 10 & ncol(ith_index)>=3) {
      #   p_cor <- corrPlot(ith_index)
      #   p_list[["Correlation"]] <- p_cor
      # }
      # output$ith_output_graphic <- renderPlot(print(p_out))

      # 1.6 DL from web ----
      output_ithindex <- ""
      if(input_ith_algorithm() == "jaccardSimilarity") output_ithindex <- "jaccardSimilarity"
      if(input_ith_algorithm() == "MAD") output_ithindex <- "MATH"
      if(input_ith_algorithm() == "mutationEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "Ratio") output_ithindex <- "pClonality"

      if(input_ith_algorithm() == "segmentCNH") output_ithindex <- "CNHplus"
      if(input_ith_algorithm() == "segmentEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "EstimateClonality") output_ithindex <- "EstimateClonality"

      if(input_ith_algorithm() == "expressionEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "Diversity_Multiregional_scRNAseq") output_ithindex <- "DiversityScore"
      if(input_ith_algorithm() == "SD") output_ithindex <- "Standard deviations"
      if(input_ith_algorithm() == "sMPD_Multiregional") output_ithindex <- "sMPD"

      if(input_ith_algorithm() == "methylationEntropy") output_ithindex <- "ShannonEntropy"
      # if(input_ith_algorithm() == "SD") output_ithindex <- "Standard deviations"

      # output[["dl_ith_output_table"]] <- downloadHandler(
      #   filename = function() {
      #     paste0(ith_prefix, "_", output_ithindex, "_", Sys.Date(), ".txt", sep = "")
      #   },
      #   content = function(file) {
      #     readr::write_delim(x = ith_index, path = file, delim = "\t")
      #   }
      # )

      # output[["dl_ith_output_graphic"]] <- downloadHandler(
      #   filename = function() {
      #     paste0(ith_prefix, "_", ith_name, "_", Sys.Date(), ".pdf", sep = "")
      #   },
      #   content = function(file) {
      #     ggsave(filename = file, p_out, width = 280, height = 140, units = "mm")
      #   }
      # )

      # 1.7 Print in web ----
      # output$ui_ith_output_table <- renderUI({
      #   shinycssloaders::withSpinner(DTOutput("ith_output_table"))
      # })
      # output$dl_ui_ith_output_table <- renderUI({
      #   downloadButton(
      #     outputId = "dl_ith_output_table",
      #     label = "DLTable"
      #   )
      # })

      # 改用Tab展示
      ithidxs_tbl <- names(v_list)
      output$ui_ith_output_table <- renderUI({
        tabs <- lapply(ithidxs_tbl, function(idx) {
          tabPanel(
            title = idx,
            uiOutput(paste0(idx,"@table"))
          )
        })
        do.call(tabsetPanel, tabs)
      })
      lapply(ithidxs_tbl, function(idx){
        output[[paste0(idx,"@table")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(DTOutput(idx)),
            downloadButton(paste0(idx, "@DLTbl"),
                           "DLTable",
                           style = "display:inline-block;float:left;"
            )
          )
        })
        output[[idx]] <- renderDT(
          v_list[[idx]],
          rownames = FALSE,
          options = list( # dom = 't',
            pageLength = 20,
            scrollY = "300px",
            scrollX = TRUE)
        )

        output[[paste0(idx, "@DLTbl")]] <- downloadHandler(
          filename = function() {
            paste0(ith_prefix, "_Table_", output_ithindex, "_", Sys.Date(), ".txt", sep = "")
          },
          content = function(file) {
            readr::write_delim(x = v_list[[idx]], path = file, delim = "\t")
          }
        )
      })


      # output$ui_ith_output_graphic <- renderUI({
      #   shinycssloaders::withSpinner(plotOutput("ith_output_graphic",
      #     height = "350px",
      #     width = "100%"
      #   ))
      # })
      # output$dl_ui_ith_output_graphic <- renderUI({
      #   downloadButton(
      #     outputId = "dl_ith_output_graphic",
      #     label = "Download"
      #   )
      # })
      # 改用Tab展示
      ithidxs <- names(p_list)
      output$ui_ith_output_graphic <- renderUI({
        tabs <- lapply(ithidxs, function(idx) {
          tabPanel(
            title = idx,
            uiOutput(paste0(idx,"@density"))
          )
        })
        do.call(tabsetPanel, tabs)
      })
      # for loop is not available
      lapply(ithidxs, function(idx){
        output[[paste0(idx,"@density")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(plotOutput(paste0(idx)
                                                    # ,height = "350px",width = '100%'
                                                    )
                                         ),
            downloadButton(paste0(idx, "@DL"),
                           "DLGraph",
                           style = "display:inline-block;float:right;"
                           )
          )
        })

        output[[paste0(idx)]] <- renderPlot({
          print(p_list[[idx]])
        })

        output[[paste0(idx, "@DL")]] <- downloadHandler(
          filename = function() {
            paste0(ith_prefix, "_Plot_", output_ithindex, "_", Sys.Date(), ".pdf", sep = "")
          },
          content = function(file) {
            ggsave(filename = file,
                   p_list[[idx]]
                   , width = 280, height = 140, units = "mm")
          }
        )
      })

      remove_modal_progress()
    })
    # demo end -----------------------------------------------------------------

    # 2. run analysis ----------------------------------------------------------
    observeEvent(input$run_analysis, {
      show_modal_progress_line(
        color = "#417AC7", # DF0101,00A064
        trail_color = "#eee",
        duration = 90,
        easing = "easeOut",
        text = "Starting computation ..."
      )

      # 2.1 parameter ----
      input_ith_data_type <- reactive({
        selected_data_type <- input$ith_data_type
        return(selected_data_type)
      })
      message("input -- user selected data types: ", input_ith_data_type())

      input_ith_algorithm <- reactive({
        selected_algorithm <- input$ith_algorithm
        return(selected_algorithm)
      })
      message("input -- user selected algorithm: ", input_ith_algorithm())
      ## 1. Muation
      ## parameter of MATH
      input_mad_ccf <- reactive({
        selected_mad_ccf <- input$mad_ccf
        return(selected_mad_ccf)
      })
      if (input_mad_ccf() == "no")  input_mad_ccf_value <- FALSE
      if (input_mad_ccf() == "yes") input_mad_ccf_value <- TRUE

      ## parameter of Shannon entropy
      input_entropy_ccf <- reactive({
        selected_entropy_ccf <- input$entropy_ccf
        return(selected_entropy_ccf)
      })
      if (input_entropy_ccf() == "no")  input_entropy_ccf_value <- FALSE
      if (input_entropy_ccf() == "yes") input_entropy_ccf_value <- TRUE
      input_entropy_nbin <- reactive({
        selected_entropy_nbin <- input$entropy_nbin
        return(selected_entropy_nbin)
      })
      input_entropy_adjusted <- reactive({
        selected_entropy_adjusted <- input$entropy_adjusted
        return(selected_entropy_adjusted)
      })
      if (input_entropy_adjusted() == "no")  input_entropy_adjusted_value <- FALSE
      if (input_entropy_adjusted() == "yes") input_entropy_adjusted_value <- TRUE
      input_entropy_weighted <- reactive({
        selected_entropy_weighted <- input$entropy_weighted
        return(selected_entropy_weighted)
      })
      if (input_entropy_weighted() == "no")  input_entropy_weighted_value <- FALSE
      if (input_entropy_weighted() == "yes") input_entropy_weighted_value <- TRUE
      input_entropy_weighted_vector <- reactive({
        raw_weight <- input$entropy_weighted_value
        if (anyNA(raw_weight) || raw_weight == "") raw_weight <- c(rep("1",input_entropy_nbin()))
        selected_entropy_weighted_vector <- text_to_numeric(raw_weight)
        return(selected_entropy_weighted_vector)
      })

      ## parameter of Ratio
      input_ratio_cutoff <- reactive({
        selected_ratio_cutoff <- input$ratio_cutoff
        return(selected_ratio_cutoff)
      })
      ## 2. CNV
      ## parameter of Shannon entropy
      input_segment_entropy_minprobes <- reactive({
        selected_segment_entropy_minprobes <- input$segment_entropy_minprobes
        return(selected_segment_entropy_minprobes)
      })
      input_segment_entropy_minkb <- reactive({
        selected_segment_entropy_minkb <- input$segment_entropy_minkb
        return(selected_segment_entropy_minkb)
      })
      input_segment_entropy_nbin <- reactive({
        selected_segment_entropy_nbin <- input$segment_entropy_nbin
        return(selected_segment_entropy_nbin)
      })

      ## 3. RNA expression/Protein
      ## parameter of Shannon entropy
      input_expression_entropy_normalized <- reactive({
        selected_expression_entropy_normalized <- input$expression_entropy_normalized
        return(selected_expression_entropy_normalized)
      })
      if (input_expression_entropy_normalized() == "no")  input_expression_entropy_normalized_value <- FALSE
      if (input_expression_entropy_normalized() == "yes") input_expression_entropy_normalized_value <- TRUE
      ## parameter of PCA
      input_pca_is_scrnaseq <- reactive({
        selected_is_scrnaseq <- input$is_scrnaseq
        return(selected_is_scrnaseq)
      })
      if (input_pca_is_scrnaseq() == "no")  input_pca_is_scrnaseq_value <- FALSE
      if (input_pca_is_scrnaseq() == "yes") input_pca_is_scrnaseq_value <- TRUE
      input_pca_expression_toppc <- reactive({
        selected_pca_expression_toppc <- input$pca_expression_toppc
        return(selected_pca_expression_toppc)
      })
      if (input_pca_expression_toppc() == "no")  input_pca_expression_toppc_value <- FALSE
      if (input_pca_expression_toppc() == "yes") input_pca_expression_toppc_value <- TRUE
      input_pca_expression_pc_number <- reactive({
        selected_pca_expression_pc_number <- input$pca_expression_pc_number
        return(selected_pca_expression_pc_number)
      })

      selected_pca_expression_rm_outlier <- reactive({
        selected_pca_expression_rm_outlier <- input$pca_expression_rm_outlier
        return(selected_pca_expression_rm_outlier)
      })
      if (selected_pca_expression_rm_outlier() == "no")  selected_pca_expression_rm_outlier_value <- FALSE
      if (selected_pca_expression_rm_outlier() == "yes") selected_pca_expression_rm_outlier_value <- TRUE
      input_pca_expression_sd_times <- reactive({
        selected_pca_expression_sd_times <- input$pca_expression_sd_times
        return(selected_pca_expression_sd_times)
      })
      input_sample_drop_percent <- reactive({
        selected_sample_drop_percent <- input$sample_drop_percent
        return(selected_sample_drop_percent)
      })

      ## parameter of Distance
      input_distance_expression_toppc <- reactive({
        selected_distance_expression_toppc <- input$distance_expression_toppc
        return(selected_distance_expression_toppc)
      })
      if (input_distance_expression_toppc() == "no")  input_distance_expression_toppc_value <- FALSE
      if (input_distance_expression_toppc() == "yes") input_distance_expression_toppc_value <- TRUE
      input_distance_expression_pc_number <- reactive({
        selected_distance_expression_pc_number <- input$distance_expression_pc_number
        return(selected_distance_expression_pc_number)
      })
      input_distance_expression_method <- reactive({
        selected_distance_expression_method <- input$distance_expression_method
        return(selected_distance_expression_method)
      })
      # 2.2 input file ----
      ## 1. Muation
      ## file for Jaccard Similarity
      input_file_mutation_jaccard <- reactive({

        if (is.null(values$upload_state_mutation_jaccard)) {
          selected_datapath <- NULL
        } else if (values$upload_state_mutation_jaccard == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_mutation_jaccard$datapath
        }
        return(selected_datapath)

      })
      ## file for MATH
      input_file_mutation_mad <- reactive({
        if (is.null(values$upload_state_mutation_mad)) {
          selected_datapath <- NULL
        } else if (values$upload_state_mutation_mad == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_mutation_mad$datapath
        }
        return(selected_datapath)
      })
      ## file for Shannon entropy
      input_file_mutation_entropy <- reactive({
        if (is.null(values$upload_state_mutation_entropy)) {
          selected_datapath <- NULL
        } else if (values$upload_state_mutation_entropy == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_mutation_entropy$datapath
        }
        return(selected_datapath)
      })
      ## file for Ratio
      input_file_mutation_ratio <- reactive({
        if (is.null(values$upload_state_mutation_ratio)) {
          selected_datapath <- NULL
        } else if (values$upload_state_mutation_ratio == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_mutation_ratio$datapath
        }
        return(selected_datapath)
      })
      ## 2. CNV
      ## file for Distance
      input_file_segment_distance <- reactive({
        if (is.null(values$upload_state_segment_distance)) {
          selected_datapath <- NULL
        } else if (values$upload_state_segment_distance == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_segment_distance$datapath
        }
        return(selected_datapath)
      })
      input_file_purity_ploidy <- reactive({
        if (is.null(values$upload_state_purity_ploidy)) {
          selected_datapath <- NULL
        } else if (values$upload_state_purity_ploidy == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_purity_ploidy$datapath
        }
        return(selected_datapath)
      })
      ## file for Shannon entropy
      input_file_segment_entropy <- reactive({
        if (is.null(values$upload_state_segment_entropy)) {
          selected_datapath <- NULL
        } else if (values$upload_state_segment_entropy == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_segment_entropy$datapath
        }
        return(selected_datapath)
      })
      ## file for EstimateClonality
      input_file_ec_segment <- reactive({
        if (is.null(values$upload_state_ec_segment)) {
          selected_datapath <- NULL
        } else if (values$upload_state_ec_segment == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_ec_segment$datapath
        }
        return(selected_datapath)
      })
      input_file_ec_segment_name <- reactive({
        if (is.null(values$upload_state_ec_segment)) {
          selected_name <- NULL
        } else if (values$upload_state_ec_segment == "reset") {
          selected_name <- NULL
        } else {
          selected_name <- input$file_ec_segment$name
        }
        return(selected_name)
      })
      input_file_ec_mutation <- reactive({
        if (is.null(values$upload_state_ec_mutation)) {
          selected_datapath <- NULL
        } else if (values$upload_state_ec_mutation == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_ec_mutation$datapath
        }
        return(selected_datapath)
      })
      ## 3. RNA expression/Protein
      ## file for Shannon entropy
      input_file_expression_entropy <- reactive({
        if (is.null(values$upload_state_expression_entropy)) {
          selected_datapath <- NULL
        } else if (values$upload_state_expression_entropy == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_expression_entropy$datapath
        }
        return(selected_datapath)
      })
      ## file for PCA
      input_file_expression_pca <- reactive({
        if (is.null(values$upload_state_expression_pca)) {
          selected_datapath <- NULL
        } else if (values$upload_state_expression_pca == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_expression_pca$datapath
        }
        return(selected_datapath)
      })
      input_file_meta_pca <- reactive({
        if (is.null(values$upload_state_meta_pca)) {
          selected_datapath <- NULL
        } else if (values$upload_state_meta_pca == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_meta_pca$datapath
        }
        return(selected_datapath)
      })
      input_file_coordinate_pca <- reactive({
        if (is.null(values$upload_state_coordinate_pca)) {
          selected_datapath <- NULL
        } else if (values$upload_state_coordinate_pca == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_coordinate_pca$datapath
        }
        return(selected_datapath)
      })
      ## file for SD
      input_file_expression_sd <- reactive({
        if (is.null(values$upload_state_expression_sd)) {
          selected_datapath <- NULL
        } else if (values$upload_state_expression_sd == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_expression_sd$datapath
        }
        return(selected_datapath)
      })
      ## file for Distance
      input_file_expression_distance <- reactive({
        if (is.null(values$upload_state_expression_distance)) {
          selected_datapath <- NULL
        } else if (values$upload_state_expression_distance == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_expression_distance$datapath
        }
        return(selected_datapath)
      })
      input_file_expression_distance_meta <- reactive({
        if (is.null(values$upload_state_expression_distance_meta)) {
          selected_datapath <- NULL
        } else if (values$upload_state_expression_distance_meta == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_expression_distance_meta$datapath
        }
        return(selected_datapath)
      })
      ## 4. Methylation
      ## file for Shannon entropy
      input_file_methylation_entropy <- reactive({
        if (is.null(values$upload_state_methylation_entropy)) {
          selected_datapath <- NULL
        } else if (values$upload_state_methylation_entropy == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_methylation_entropy$datapath
        }
        return(selected_datapath)
      })
      ## file for SD
      input_file_methylation_sd <- reactive({
        if (is.null(values$upload_state_methylation_sd)) {
          selected_datapath <- NULL
        } else if (values$upload_state_methylation_sd == "reset") {
          selected_datapath <- NULL
        } else {
          selected_datapath <- input$file_methylation_sd$datapath
        }
        return(selected_datapath)
      })

      # 2.3 ITH计算 ----
      ith_index <- data.frame()
      # 1. Mutation(VAF/CCF)
      # 1. Mutation(VAF/CCF)
      if (input_ith_data_type() == "VAF_CCF") {
        if (input_ith_algorithm() == "jaccardSimilarity"){
          if (input_file_mutation_jaccard() == "" || is.null(input_file_mutation_jaccard())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_mutation_jaccard())
            in_data <- data.table::fread(input_file_mutation_jaccard(), encoding = "UTF-8")
            data_cols <- c("PatientID", "SampleID", "Hugo_Symbol")
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the jaccard similarity!")
              ith_index <- lpy_JaccardSimilarity(in_data)
            }
          }
        }
        if (input_ith_algorithm() == "MAD"){
          if (input_file_mutation_mad() == "" || is.null(input_file_mutation_mad())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_mutation_mad())
            in_data <- data.table::fread(input_file_mutation_mad(), encoding = "UTF-8")
            if(input_mad_ccf_value){
              data_cols <- c("SampleID", "CCF")
            }else{
              data_cols <- c("SampleID", "Tumor_VAF")
            }
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the MATH!")
              ith_index <- lpy_calcMATH(in_data,cancer_cell_fraction = input_mad_ccf_value)
            }
          }
        }
        if (input_ith_algorithm() == "mutationEntropy"){

          if (input_file_mutation_entropy() == "" || is.null(input_file_mutation_entropy())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_mutation_entropy())
            in_data <- data.table::fread(input_file_mutation_entropy(), encoding = "UTF-8")
            if(input_entropy_ccf_value){
              data_cols <- c("SampleID", "CCF")
            }else{
              data_cols <- c("SampleID", "Tumor_VAF")
            }
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the Shannon entropy!")
              ith_index <- lpy_calcMutationEntropy(mutation_data = in_data,
                                                  cancer_cell_fraction = input_entropy_ccf_value,
                                                  based = exp(1),
                                                  nbins = input_entropy_nbin(),
                                                  start = 0,
                                                  end = 1,
                                                  adjusted = input_entropy_adjusted_value,
                                                  weighted = input_entropy_weighted_value,
                                                  given_weights = input_entropy_weighted_vector())
            }
          }

        }
        if (input_ith_algorithm() == "Ratio"){

          if (input_file_mutation_ratio() == "" || is.null(input_file_mutation_ratio())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_mutation_ratio())
            in_data <- data.table::fread(input_file_mutation_ratio(), encoding = "UTF-8")
            data_cols <- c("SampleID", "CCF")
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the Clonal ratio!")
              ith_index <- lpy_PClonality(ccf_data = in_data,
                                               clonal_ratio_ccf  = input_ratio_cutoff())
            }
          }
        }
      }
      # 2. CNV
      if(input_ith_data_type() == "Segment"){
        if (input_ith_algorithm() == "segmentCNH"){

          if (input_file_segment_distance() == "" || is.null(input_file_segment_distance())) {
            shinyalert::shinyalert(
              title = "Missing segment file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file[segment]: ", input_file_segment_distance())
            message("input -- user upload file[purity and ploidy]: ", input_file_purity_ploidy())
            runCNH <- TRUE
            in_data_seg <- data.table::fread(file = input_file_segment_distance(), encoding = "UTF-8") %>% as.data.frame()
            data_cols <- c("SampleID", "Start", "End", "Segment_Mean")
            if (!is.data.frame(in_data_seg) | length(grep(FALSE, data_cols %in% colnames(in_data_seg))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
              runCNH <- FALSE
            }
            if (!(is.numeric(in_data_seg$Start) &
                  is.numeric(in_data_seg$End) &
                  is.numeric(in_data_seg$Segment_Mean))) {
              shinyalert::shinyalert(
                title = "Error type of field!",
                text = paste(paste(c("Start", "End", "Segment_Mean"), collapse=" "),"must be numeric!"),
                type = "error"
              )
              runCNH <- FALSE
            }
            if(is.null(input_file_purity_ploidy())){
              in_data_pp = NULL
            }else{
              in_data_pp <- data.table::fread(file = input_file_purity_ploidy(), encoding = "UTF-8") %>% as.data.frame()
              data_cols <- c("SampleID", "Ploidy", "Purity")
              if (!is.data.frame(in_data_pp) | length(grep(FALSE, data_cols %in% colnames(in_data_pp))) != 0) {
                shinyalert::shinyalert(
                  title = "Missing mandatory fields!",
                  text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                  type = "error"
                )
                runCNH <- FALSE
              }
              if (!(is.numeric(in_data_pp$Ploidy) & is.numeric(in_data_pp$Purity))){
                shinyalert::shinyalert(
                  title = "Error type of field!",
                  text = paste(paste(c("Ploidy", "Purity"), collapse=" "),"must be numeric!"),
                  type = "error"
                )
                runCNH <- FALSE
              }
              same_sids <- intersect(unique(in_data_seg$SampleID),in_data_pp$SampleID)
              if (length(same_sids) == 0) {
                shinyalert::shinyalert(
                  title = "Error, no match SampleID!",
                  text = "No identical SampleID between segment data and purity/ploidy data",
                  type = "error"
                )
                runCNH <- FALSE
              }
            }
            if(runCNH){
              message("input -- calculate the CNH and CNHplus!")

              is_cnhplus <- TRUE
              ith_cnhplus <- lpy_calcCNHplus(seg_data = in_data_seg,pp_data = in_data_pp,plus = is_cnhplus)
              ith_index <- ith_cnhplus %>%
                dplyr::select(SampleID,CNHplus)

              # 数值很小，绘图会报错！暂时不计算
              # ith_cnh <- lpy_calcCNH(seg_data = in_data_seg,pp_data = in_data_pp)
              # ith_index <- ith_cnh %>%
              #   dplyr::select(SampleID,CNH) %>%
              #   dplyr::full_join(ith_cnhplus %>%
              #                      dplyr::select(SampleID,CNHplus),
              #                    by='SampleID')%>%
              #   dplyr::select(SampleID,CNHplus,CNH)

            }
          }

        }
        if (input_ith_algorithm() == "segmentEntropy"){

          if (input_file_segment_entropy() == "" || is.null(input_file_segment_entropy())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file[segment]: ", input_file_segment_entropy())
            in_data_seg <- data.table::fread(input_file_segment_entropy()) %>% as.data.frame()

            if ("logR" %in% colnames(in_data_seg)) {
              data_cols <- c("SampleID", "Chr", "Start", "End", "nProbes", "mBAF", "logR")
              if (!is.data.frame(in_data_seg) | length(grep(FALSE, data_cols %in% colnames(in_data_seg))) != 0) {
                shinyalert::shinyalert(
                  title = "Missing mandatory fields!",
                  text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                  type = "error"
                )
              }else{
                message("input -- calculate the Shannon entropy[segment logR]!")
                ith_index <- ldy_snpDiv(seg_data = in_data_seg,
                                        minprobes = input_segment_entropy_minprobes(),
                                        minkb = input_segment_entropy_minkb(),
                                        nbins = input_segment_entropy_nbin())
                ith_index %<>%
                  dplyr::select(SampleID,`Shannon entropy` = Shannon_index,`Ripley’s K` = Ripley_index)
              }
            }else if ("Segment_Mean" %in% colnames(in_data_seg)) {
              data_cols <- c("SampleID", "Chr", "Start", "End", "nProbes", "Segment_Mean")
              if (!is.data.frame(in_data_seg) | length(grep(FALSE, data_cols %in% colnames(in_data_seg))) != 0) {
                shinyalert::shinyalert(
                  title = "Missing mandatory fields!",
                  text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                  confirmButtonCol = "#AEDEF4",
                  type = "error"
                )
              }else{
                message("input -- calculate the Shannon entropy[segment CBS]!")
                ith_index <- ldy_cbsDiv(seg_data = in_data_seg,
                                        minprobes = input_segment_entropy_minprobes(),
                                        minkb = input_segment_entropy_minkb(),
                                        nbins = input_segment_entropy_nbin())
                ith_index %<>%
                  dplyr::select(SampleID,`Shannon entropy` = Shannon_index)
              }

            }else{
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = "",
                type = "error"
              )
            }

          }

        }
        if (input_ith_algorithm() == "EstimateClonality"){

          if (input_file_ec_mutation() == "" || is.null(input_file_ec_mutation()) ||
              input_file_ec_segment() == ""  || is.null(input_file_ec_segment())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload mutation and segment file.",
              type = "error"
            )
          }else{
            message("input -- user upload file[mutation]: ", input_file_ec_mutation())
            message("input -- user upload file[segment]: ", input_file_ec_segment())

            tmp_path <- paste0(ith_path, input_file_ec_segment_name(), "_",as.numeric(Sys.time()))
            dir.create(tmp_path)
            seg_list_file <- here::here(paste0(tmp_path, "/", "segments_estimateclonality.rdata"))
            if (!file.exists(seg_list_file)) {
              seg_file <- input_file_ec_segment()
              in_data_seg <- data.table::fread(seg_file) %>% as.data.frame()
              list_seg <- list("segments" = in_data_seg)
              save(list_seg, file = seg_list_file)
            }
            # out_dir <- paste0(tmp_path, "/")
            ith_index <- calcEstimateClonality(mutation_file = input_file_ec_mutation(),
                                               seg_file = seg_list_file,
                                               out_dir = paste0(tmp_path, "/"))

            ith_index %<>%
              dplyr::select(SampleID,pLM = Late)
            unlink(tmp_path, recursive = T)
          }

        }
      }
      # 3. RNAexpression/Protein
      if(input_ith_data_type() == "Expression"){
        if (input_ith_algorithm() == "expressionEntropy"){

          if (input_file_expression_entropy() == "" || is.null(input_file_expression_entropy())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_expression_entropy())
            in_data <- data.table::fread(input_file_expression_entropy(), encoding = "UTF-8")
            data_cols <- c("gene_id")
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields (1st column): ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the Diversity score!")
              in_data <- in_data %>%
                tibble::column_to_rownames(var = 'gene_id')
              ith_index <- data.frame("Diversity_score" = calcExprEntropy(in_data,
                                                                          norm = input_expression_entropy_normalized_value)) %>%
                tibble::rownames_to_column(var = 'SampleID') %>%
                dplyr::select(SampleID,`Shannon entropy` = Diversity_score)
            }
          }
        }
        if (input_ith_algorithm() == "Diversity_Multiregional_scRNAseq"){

          if (input_file_expression_pca() == "" || is.null(input_file_expression_pca())||
              input_file_meta_pca() == "" || is.null(input_file_meta_pca())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file[expr]: ", input_file_expression_pca())
            message("input -- user upload file[meta]: ", input_file_meta_pca())

            in_data <- data.table::fread(input_file_expression_pca(), encoding = "UTF-8")
            in_meta <- data.table::fread(input_file_meta_pca(), encoding = "UTF-8")
            runDiv <- TRUE

            data_cols <- c("hugo_symbol")
            if(!is.data.frame(in_data) | length(grep(FALSE, c("hugo_symbol") %in% colnames(in_data))) != 0){
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields of expr file: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
              runDiv <- FALSE
            }
            data_cols <- c("PatientID", "SampleID")
            if (!is.data.frame(in_meta) | length(grep(FALSE, data_cols %in% colnames(in_meta))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields of meta file: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
              runDiv <- FALSE
            }
            if (length(intersect(colnames(in_data),in_meta$SampleID)) == 0) {
              shinyalert::shinyalert(
                title = "Error, no match SampleID!",
                text = "No identical SampleID between expr data and meta data",
                type = "error"
              )
              runDiv <- FALSE
            }
            if(input_pca_is_scrnaseq_value){
              # not need coordinate data
              in_coord = NULL
            }else if(input_file_coordinate_pca() == "" ||
                     is.null(input_file_coordinate_pca())){
              # do not update coordinate data
              in_coord = NULL
            }else{
              # update coordinate data
              in_coord <- data.table::fread(input_file_coordinate_pca())
              data_cols <- c('PatientID','SampleID',"x", "y")
              if (!is.data.frame(in_coord) | length(grep(FALSE, data_cols %in% colnames(in_coord))) != 0) {
                shinyalert::shinyalert(
                  title = "Missing mandatory fields!",
                  text = paste0("Mandatory fields of coordinate file: ",paste(data_cols, collapse=", ")),
                  type = "error"
                )
                runDiv <- FALSE
              }
              if (length(intersect(colnames(in_data),in_coord$SampleID)) == 0) {
                shinyalert::shinyalert(
                  title = "Error, no match SampleID!",
                  text = "No identical SampleID between expr data and coordinate data",
                  type = "error"
                )
                runDiv <- FALSE
              }
            }
            if(runDiv){
              message("input -- calculate the Diversity score!")
              ith_index <- calcPcaDiversity(in_data,
                                            in_meta,
                                            topPC = input_pca_expression_toppc_value,
                                            nPCs = input_pca_expression_pc_number(),
                                            rmOutlier = selected_pca_expression_rm_outlier_value,
                                            sdTimes = input_pca_expression_sd_times(),
                                            sampleDropPercent = input_sample_drop_percent(),
                                            coordinate = in_coord)
            }
          }
        }
        if (input_ith_algorithm() == "SD"){

          if (input_file_expression_sd() == "" || is.null(input_file_expression_sd())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_expression_sd())
            in_data <- data.table::fread(input_file_expression_sd(), encoding = "UTF-8")
            data_cols <- c('gene_id')
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields (1st column): ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the SD!")
              ith_index <- calcSD(in_data,adjusted = T,imputed = T)
              ith_index %<>%
                dplyr::select(SampleID,
                              `Standard deviations` = mSD)
            }
          }

        }
        if (input_ith_algorithm() == "sMPD_Multiregional"){

          if (input_file_expression_distance() == "" || is.null(input_file_expression_distance())||
              input_file_expression_distance_meta() == "" || is.null(input_file_expression_distance_meta())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file[expr]: ", input_file_expression_distance())
            message("input -- user upload file[meta]: ", input_file_expression_distance_meta())
            in_data <- data.table::fread(input_file_expression_distance(), encoding = "UTF-8")
            in_meta <- data.table::fread(input_file_expression_distance_meta(), encoding = "UTF-8")

            runMPD <- TRUE

            data_cols <- c("PatientID", "SampleID")
            if (!is.data.frame(in_meta) | length(grep(FALSE, data_cols %in% colnames(in_meta))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields (1st column): ",paste(data_cols, collapse=", ")),
                type = "error"
              )
              runMPD <- FALSE
            }
            data_cols <- c('gene_id')
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields (1st column): ",paste(data_cols, collapse=", ")),
                type = "error"
              )
              runMPD <- FALSE
            }
            if (length(intersect(colnames(in_data),in_meta$SampleID)) == 0) {
              shinyalert::shinyalert(
                title = "Error, no match SampleID!",
                text = "No identical SampleID between expr data and meta data",
                type = "error"
              )
              runMPD <- FALSE
            }

            if(runMPD){
              message("input -- calculate the sMPD!")
              ith_index <- lpy_MPD(in_data,in_meta,
                                               pca = input_distance_expression_toppc_value,
                                               nPCs = input_distance_expression_pc_number(),
                                               method = input_distance_expression_method())
              ith_index$sMPD <- func_2zeroone(ith_index$MPD) %>% round(4)
              ith_index %<>%
                dplyr::select(PatientID,sMPD)
            }
          }

        }
      }
      # 4. Methylation
      if(input_ith_data_type() == "Methylation") {
        if (input_ith_algorithm() == "methylationEntropy"){
          if (input_file_methylation_entropy() == "" || is.null(input_file_methylation_entropy())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_methylation_entropy())
            in_data <- data.table::fread(input_file_methylation_entropy(), encoding = "UTF-8")
            data_cols <- c("gene_id")
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the MethylEntropy!")
              ith_index <- ldy_calcMethylEntropy(in_data)
            }
          }
        }
        if (input_ith_algorithm() == "SD"){
          if (input_file_methylation_sd() == "" || is.null(input_file_methylation_sd())) {
            shinyalert::shinyalert(
              title = "Missing file!",
              text = "Please upload your data file.",
              type = "error"
            )
          }else{
            message("input -- user upload file: ", input_file_methylation_sd())
            in_data <- data.table::fread(input_file_methylation_sd(), encoding = "UTF-8")
            data_cols <- c("gene_id")
            if (!is.data.frame(in_data) | length(grep(FALSE, data_cols %in% colnames(in_data))) != 0) {
              shinyalert::shinyalert(
                title = "Missing mandatory fields!",
                text = paste0("Mandatory fields: ",paste(data_cols, collapse=", ")),
                type = "error"
              )
            }else{
              message("input -- calculate the MethylEntropy!")
              ith_index <- calcSD(in_data)
              ith_index %<>%
                dplyr::select(SampleID,
                              `Standard deviations` = mSD)
            }
          }
        }
      }

      # 2.3 Table Visualized into web
      v_list <- list("ITHindex" = ith_index)
      # output$ith_output_table <- renderDT(ith_index,
      #   rownames = FALSE,
      #   options = list( # dom = 't',
      #     pageLength = 20,
      #     scrollY = "300px",
      #     scrollX = TRUE
      #   )
      # )
      # density plot
      # browser()
      p_list <- list("Graphic" = p_less)
      # more than 10 records, the density will be plot
      if (nrow(ith_index) >= 8) p_list <- densityPlot(ith_index)
      # more than 2 indexs, the corr will be plot
      if(nrow(ith_index) >= 8 & ncol(ith_index)>=3) {
        p_cor <- corrPlot(ith_index)
        p_list[["Correlation"]] <- p_cor
      }

      # output$ith_output_graphic <- renderPlot(print(p_out))

      output[["dl_ith_output_table"]] <- downloadHandler(
        filename = function() {
          paste0(ith_prefix, "_", input_ith_algorithm(), "_", Sys.Date(), ".txt", sep = "")
        },
        content = function(file) {
          readr::write_delim(x = ith_index, path = file, delim = "\t")
        }
      )
      # output[["dl_ith_output_graphic"]] <- downloadHandler(
      #   filename = function() {
      #     paste0(ith_prefix, "_", ith_name, "_", Sys.Date(), ".pdf", sep = "")
      #   },
      #   content = function(file) {
      #     ggsave(filename = file, p_out, width = 280, height = 140, units = "mm")
      #   }
      # )

      output_ithindex <- ""
      if(input_ith_algorithm() == "jaccardSimilarity") output_ithindex <- "jaccardSimilarity"
      if(input_ith_algorithm() == "MAD") output_ithindex <- "MATH"
      if(input_ith_algorithm() == "mutationEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "Ratio") output_ithindex <- "pClonality"

      if(input_ith_algorithm() == "segmentCNH") output_ithindex <- "CNHplus"
      if(input_ith_algorithm() == "segmentEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "EstimateClonality") output_ithindex <- "EstimateClonality"

      if(input_ith_algorithm() == "expressionEntropy") output_ithindex <- "ShannonEntropy"
      if(input_ith_algorithm() == "Diversity_Multiregional_scRNAseq") output_ithindex <- "DiversityScore"
      if(input_ith_algorithm() == "SD") output_ithindex <- "Standard deviations"
      if(input_ith_algorithm() == "sMPD_Multiregional") output_ithindex <- "sMPD"

      if(input_ith_algorithm() == "methylationEntropy") output_ithindex <- "ShannonEntropy"
      # if(input_ith_algorithm() == "SD") output_ithindex <- "Standard deviations"

      # 2.4 Plot Visualized into web
      # output$ui_ith_output_table <- renderUI({
      #   shinycssloaders::withSpinner(DTOutput("ith_output_table"))
      # })
      # output$dl_ui_ith_output_table <- renderUI({
      #   downloadButton(
      #     outputId = "dl_ith_output_table",
      #     label = "Download"
      #   )
      # })
      # 改用Tab展示
      ithidxs_tbl <- names(v_list)
      output$ui_ith_output_table <- renderUI({
        tabs <- lapply(ithidxs_tbl, function(idx) {
          tabPanel(
            title = idx,
            uiOutput(paste0(idx,"@table"))
          )
        })
        do.call(tabsetPanel, tabs)
      })
      lapply(ithidxs_tbl, function(idx){
        output[[paste0(idx,"@table")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(DTOutput(idx)),
            downloadButton(paste0(idx, "@DLTbl"),
                           "DLTable",
                           style = "display:inline-block;float:left;"
            )
          )
        })
        output[[idx]] <- renderDT(
          v_list[[idx]],
          rownames = FALSE,
          options = list( # dom = 't',
            pageLength = 20,
            scrollY = "300px",
            scrollX = TRUE)
        )

        output[[paste0(idx, "@DLTbl")]] <- downloadHandler(
          filename = function() {
            paste0(ith_prefix, "_Table_", output_ithindex, "_", Sys.Date(), ".txt", sep = "")
          },
          content = function(file) {
            readr::write_delim(x = v_list[[idx]], path = file, delim = "\t")
          }
        )
      })


      # output$ui_ith_output_graphic <- renderUI({
      #   shinycssloaders::withSpinner(plotOutput("ith_output_graphic",
      #     height = "350px",
      #     width = "100%"
      #   ))
      # })
      # output$dl_ui_ith_output_graphic <- renderUI({
      #   downloadButton(
      #     outputId = "dl_ith_output_graphic",
      #     label = "Download"
      #   )
      # })
      ithidxs <- names(p_list)
      output$ui_ith_output_graphic <- renderUI({
        tabs <- lapply(ithidxs, function(idx) {
          tabPanel(
            title = idx,
            uiOutput(paste0(idx,"@density"))
          )
        })
        do.call(tabsetPanel, tabs)
      })
      # for loop is not available
      lapply(ithidxs, function(idx){
        output[[paste0(idx,"@density")]] <- renderUI({
          tagList(
            shinycssloaders::withSpinner(plotOutput(paste0(idx)
                                                    # ,height = "350px",width = '100%'
            )
            ),
            downloadButton(paste0(idx, "@DL"),
                           "DLGraph",
                           style = "display:inline-block;float:right;"
            )
          )
        })

        output[[paste0(idx)]] <- renderPlot({
          print(p_list[[idx]])
        })

        output[[paste0(idx, "@DL")]] <- downloadHandler(
          filename = function() {
            paste0(ith_prefix, "_Plot_", output_ithindex, "_", Sys.Date(), ".pdf", sep = "")
          },
          content = function(file) {
            ggsave(filename = file,
                   p_list[[idx]]
                   , width = 280, height = 140, units = "mm")
          }
        )
      })

      remove_modal_progress()
    })
    # analysis end -------------------------------------------------------------
  })
}
