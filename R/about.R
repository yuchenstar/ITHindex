#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : ith_about.R
# @License : Copyright(C), X
# @Author  : Wenchuan Xie
# @Time    : 2023-10-18
# @IDE     : RStudio
# @Desc    : ITH quantification information
#===============================================================================

df <- data.frame(
  question = c("Question1", "Question2", "Question3"),
  answer = c("Répondre <b>key word</b>",
             "Répondre 2 <img src=\"https://raw.githubusercontent.com/jienagu/faq/master/faq_logo.png\">",
             "Répondre 3")
)


# List contain ui and server of page
ith_about_page <- list()

about_page <- dashboardBody(
  style = "color:#111111;background-color:#ffffff;",
  ## About ----
  fluidRow(
    style = "margin-top:5%",
    box(width = 12
        ,collapsible = TRUE,collapsed = FALSE
        ,solidHeader = TRUE
        ,title = strong("About ITHindex",
                        style = "font-size:20px;color:white;"),
        background = "black",
        icon = icon("address-book"),
        style = "background-color:#ffffff;color:black;",# display:block;padding:10px;position:relative;
        p(strong("ITHindex is free and open to all users and there is no login requirement")),
        p(strong("ITHindex")," is a freely accessible web-server tool for intra-tumor heterogeneity (ITH)
           index quantification. The platform offers multiple algorithms options and
           downloadable output graphics and tables."),
        p(strong("Key Points: ")),
        p("- ITHindex integrate multiple algorithms for the quantification of intra-tumor heterogeneity."),
        p("- ITHindex can be used to evaluate multi-level ITH based on multi-omics data."),
        p("- ITHindex provide a user-friendly platform for researchers to investigate the role of ITH in oncologic biology.")
      )
    ),
  ## Contact us
  fluidRow(
    # style = "margin-top:5%",
    box(width = 12
        ,collapsible = TRUE,collapsed = FALSE
        ,solidHeader = TRUE
        ,title = strong("Contact us",
                        style = "font-size:20px;color:white;"),
        background = "black",
        icon = icon("share"),
        style = "background-color:#ffffff;color:black;",# display:block;padding:10px;position:relative;
        p("If you have any trouble accessing any information on this website or have any further questions
          or feedback relating to the tool, then please contact us:"),
        # p("Wenchuan Xie: ",tags$a(href = "wenchuan.xie@gmail.com", "wenchuan.xie@gmail.com", target = "_blank")),
        p(
          mailtoR::mailtoR(email = "liuyutao@cicams.ac.cn",
                           text = "Yutao Liu",
                           subject = "Question of ITHindex",
                           cc = c("wenchuan.xie@gmail.com"),
                           body = ""),
          " or ",
          mailtoR::mailtoR(email = "wenchuan.xie@gmail.com",
                           text = "Wenchuan Xie",
                           subject = "Question of ITHindex",
                           cc = c("wenchuan.xie@brbiotech.com"),
                           body = ""),
          use_mailtoR()
        ),
        p("and we will be happy to help."),
        p("When describing the issues,
          please attach the related screenshots and provide
          the information about the problem as detailed as possible,
          including the information of the browser you used.")
    )
  ),
  ## Acknowledgements ----
  fluidRow(
    box(width = 12
        ,collapsible = TRUE,collapsed = FALSE
        ,solidHeader = TRUE
        ,title = strong("Acknowledgment",
                        style = "font-size:20px;color:white;"),
        background = "black",
        icon = icon("share"),
        style = "background-color:#ffffff;color:black;",

        p("The development of ITHindex was made possible because of the excellent
          algorithms provided by various studies."),
        p("We also give our special thanks to ",
          tags$a(href = "https://shiny.rstudio.com/gallery/", "Shiny Gallery", target = "_blank"),
          "for sharing the excellent shiny template.")
    )
  ),
  ## Comment Box ----
  # fluidRow(
    # box(width = 12
        # ,collapsible = TRUE,collapsed = TRUE
        # ,solidHeader = TRUE
        # ,title = strong("Comment Box",
        #                 style = "font-size:20px;color:white;"),
        # background = "black",
        # icon = icon("envelope"),
        # style = "background-color:#ffffff;color:black;",# display:block;padding:10px;position:relative;

        # Comment box!
        # fluidRow(
        #   textInput(inputId = "comment_title",
        #             label = "Name/Email (Optinal)",
        #             placeholder = "Please enter your name or email ...",
        #             width = "20%")
        # ),
        # fluidRow(
        #   textAreaInput(inputId = "comment_text",
        #                 label = "Comment*",
        #                 placeholder = "Please enter your comment here...",
        #                 width = "70%",height = "100px")
        # ),
        # fluidRow(
        #   tags$head(
        #     tags$style(HTML('#submit_comment{background-color:#111;border-color:#111}'))
        #   ),
        #   style = "color:white;", # 与第一个输入框对齐
        #   actionButton(
        #     inputId = "submit_comment",
        #     label = "Submit Comment",
        #     class = "btn-primary"
        #   )
        # )
    # )
  # ),
  ## FAQ ----
  fluidRow(
    # style = "margin-top:5%",
    box(width = 12
        ,collapsible = TRUE,collapsed = FALSE
        ,solidHeader = TRUE
        ,title = strong("FAQ",
                        style = "font-size:20px;color:white;"),
        background = "black",
        icon = icon("comments"),
        style = "background-color:#ffffff;color:black;padding:0px;",# display:block;padding:10px;position:relative;
        # p("We give our special thanks to ",
        #   tags$a(href = "https://shiny.rstudio.com/gallery/", "Shiny Gallery", target = "_blank"),
        #   "for sharing the excellent template."),

        p(),
        includeMarkdown("rmd/about.Rmd"),

        # p(strong("1. What is the intra-tumor heterogeneity (ITH)?")),
        # p("Tumor heterogeneity contributes to tumor progression, metastasis and a main cause of immune evasion and therapeutic resistance in many solid tumor types.
        #   It can be classified as: intertumor heterogeneity, intersite heterogeneity and intratumor heterogeneity.
        #   Understanding the roots of tumor heterogeneity is critical in overcoming tumor relapse and chemoresistance.
        #   Here, we focus on intratumor heterogeneity (ITH), which refers to differences between cells within a tumor mass,
        #   the extent of which has been demonstrated through recent multiregion next generation sequencing analyses.
        #   High ITH is associated with poor outcome in many solid tumor types, and it may include heterogeneity for genetic,
        #   transcriptomic, proteomic and epigenetic features."),
        #
        #
        # p(strong("2. How to quantify intratumor heterogeneity?")),
        # p("Intratumor heterogeneity can be quantified by using various measurements based on the multi-omics data.
        #   Previous studies have developed a variety of methods for multi-dimensional evaluation of ITH,
        #   based on the profiling of genomic, transcriptomic and epigenetic features from single/multi-regional sequencing,
        #   spatial transcriptomics, or single-cell sequencing."),
        #
        # p(strong("3. What are the mathematical methods used to infer ITH?")),
        # p("Mathematical models for quantification of ITH can be classified into four distinct categories by
        #   the complexity and underlying model assumptions:
        #   descriptive statistics, information theory (entropy), and spatial statistics (distance),
        #   as well as radiomic metrics on texture analysis, fractal analysis and wavelets
        #   (radiomic metrics are not integrated in ITHindex)."),
        #
        # p(strong("4. What is the Jaccard Similarity index?")),
        # p("The Jaccard similarity index compares members for two sets to see which members are shared
        #    and which are distinct. It’s a measure of similarity for the two sets of data, with a range from 0% to 100%.
        #   The higher the percentage, the more similar the two populations. The formula to find the Index is:"),
        # p("Jaccard Index = (the number in both sets) / (the number in either set) * 100"),
        #
        # p(strong("5. What is the mutant-allele tumor heterogeneity (MATH)?")),
        # p("MATH is a descriptive statistics metric to quantify ITH based on the VAF distribution across all mutated loci."),
        #
        # p(strong("6. What is the proportion of clonality (pClonality)")),
        # p("The Cancer cell fraction (CCF) is the fraction of cancer cells from the sequenced sample carrying a set of SNVs,
        #   which can be inferred from PyClone or SciClone", "Generally, events appearing in all cancer cells (CCF = 100%)
        #   are considered clonal. However, this definition is not entirely consistent in other articles. Here, we define a parameter
        #   to define a cutoff of CCF to classify a mutation as clonal, and the others are subclonal. The proportion of clonality
        #   (pClonality) is calculated based on proportion of clonal in a sample."),
        #
        # p(strong("7. What is the Shannon entropy?")),
        # p("The Shannon entropy is a measure of diversity that quantifies the uncertainty in assigning
        #   the species identity of an individual in a population. It is calculated as:"),
        # p("H = -∑ Pi*ln(Pi)"),
        # p("where Pi is the frequency of species i in the population."),
        #
        # p(strong("8. What is the proportion of late-stage mutations (pLM)?")),
        # p("The proportion of late-stage mutations (pLM) is calculated based on proportion of late mutations
        #   inferred from EstimateClonality."),
        #
        # p(strong("9. What is the EstimateClonality?")),
        # p("The ",tags$a(href = "https://github.com/b-niu/EstimateClonality", "EstimateClonality", target = "_blank"),
        #   " is an R package which uses to determine the timing of SNVs
        #   for all mutations. The CCF of SNVs based on tumor purity and CNA profiles and mutation copy number.
        #   Early and late mutations were defined as a mutation copy number of >1 and <=1, respectively,
        #   based on the binomial test implemented in the package."),
        #
        # p(strong("10. What is the Copy Number intra-tumor Heterogeneity(CNH+ [CNHplus])?")),
        # p("van Dijk et al introduced a single-sample method for estimating the ITH from Copy Number Variation (CNV) data,
        #    dubbed Copy Number Heterogeneity.
        #   The ACN profiles, recovered by CNH, may contain segments with the negative number of copies; cf. (Grendár et al. 2022).
        #   CNHplus corrects this omission in CNH as it imposes the non-negativity constraint on ACN of tumor in searching for
        #   the solution of the optimization problem that defines CNH. The source code can be found in",
        #   tags$a(href = "https://github.com/grendar/CNHplus", "Here", target = "_blank")),
        #
        # p(strong("11. What is the cancer cell fraction (CCF)?")),
        # p("The CCF, also known as the cellular prevalence or the mutation cellularity,
        #   is a proportion of cancer cells in a tumor containing the single-nucleotide variant,
        #   is commonly used to quantify tumor heterogeneity and evolution."),
        #
        # p(strong("12. What is the Diversity score?")),
        # p("The ",
        #   tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6801104/", "Diversity score", target = "_blank"),
        #   "is a measurement of ITH, which was calculated based on the gene expression profiles of signle cell within a tumor.
        #    PCA was applied in the scRNAseq data, and top N (default 30) PCs was selected, the diversity score
        #   of a tumor is aclculated as the average distance of all cells within the tumor to the centroid."),
        #
        #
        #
        # p(strong("13. What is the methylation entropy?")),
        # p("The methylation entropy is used to estimate the information content stored in a given set of CpG sites.
        #   Entropy is minimized when the methylation levels of all sites are either 0% or 100% and maximized when all of them are 50%.
        #   It is calculated as:"),
        # p("Entropy =1/(N∗log(1/2))*∑[MFi∗log(MFi)+(1−MFi)∗log(1−MFi)]"),
        # p("where MFi is the methylation fraction (beta value) of the i-th methylation marker(CpG site) and N is the number of markers.")

    )
  )

)


ith_about_page$ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE,minified = FALSE),
  about_page
  )

ith_about_page$server <- function(input, output,session){

  # observeEvent(input$submit_comment, {
  #   shinyalert::shinyalert(title = "Thanks for your response!",
  #                          text = "We will get back to you as soon as possible.",
  #                          type = "success")
  # })

}
