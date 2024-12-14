
ith_data_page <- list()

data_page <- dashboardBody(
  id = "data_page_id",
  style = "color:#111111;background-color:#ffffff;",
  fluidRow(
    style = "margin-top:5%",
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE
      # ,status = "warning"
      , solidHeader = TRUE,
      title = strong("ITH index of pan-cancer from TCGA",
                     style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;",
      downloadButton("tcga_data_dl","Download"),
      dataTableOutput('tcga_data_table')
    )
  ),
  fluidRow(
    style = "margin-top:1%",
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE
      # ,status = "warning"
      , solidHeader = TRUE,
      title = strong("ITH (scale value) difference in pan-cancer from TCGA",
                     style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;",
      uiOutput(outputId = "tcga_data_display"),
    )
  ),
  fluidRow(
    style = "margin-top:1%",
    box(
      width = 12,
      collapsible = TRUE, collapsed = FALSE
      # ,status = "warning"
      , solidHeader = TRUE,
      title = strong("ITH (raw value) difference in pan-cancer from TCGA",
                     style = "font-size:20px;color:white;"
      ),
      background = "black",
      style = "background-color:#ffffff;color:black;",
      uiOutput(outputId = "tcga_mutation_display"),
      uiOutput(outputId = "tcga_segment_display"),
      uiOutput(outputId = "tcga_expression_display"),
      uiOutput(outputId = "tcga_methylation_display")
    )
  )
)

ith_data_page$ui <- dashboardPage(
  dashboardHeader(disable = TRUE),
  dashboardSidebar(disable = TRUE, minified = FALSE),
  data_page
)


ith_data_page$server <- function(input, output, session) {

  # 数据
  #
  # Table 展示
  output[["tcga_data_table"]] <- renderDataTable({

    datatable(tcga_data,
              callback = JS("$('div.dwnld').append($('#dl_csv'));"),
              # extensions = c("Buttons"),
              filter = 'top',
              style = 'bootstrap',
              rownames = FALSE,
              selection = list(mode = "single", target = "row"), # target = "cell"：选择单元格
              options = list(scrollX = TRUE,
                             keys = TRUE,
                             autoWidth = TRUE ,
                             paging = TRUE,   # 分页
                             searchHighlight = TRUE,
                             orderClasses = TRUE,
                             dom = 'B<"dwnld">frtip',
                             # dom = 'Bfrtip',
                             # buttons =  c('copy'),
                             language = list(
                               zeroRecords = "No records found matching your selection"),
                             columnDefs = list(list(className = 'dt-center',
                                                    targets = "_all"),
                                               list(width = '100px',
                                                    targets = c(0,1)) # 第一、二列
                                               ))
              )
  })
  # Table 下载
  output[['tcga_data_dl']] <- downloadHandler(
    filename = function() {
      paste0(ith_prefix, "_", "TCGA", "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tcga_data, file)
    }
  )

  # Plot展示
  output[["tcga_data_display"]] <- renderUI({
    shinycssloaders::withSpinner(plotOutput(paste0("tcga_data_plot")))
  })
  output[["tcga_data_plot"]] <- renderPlot(print(p_tcga_box))

  output[["tcga_mutation_display"]] <- renderUI({
    shinycssloaders::withSpinner(plotOutput(paste0("tcga_mutation_plot")))
  })
  output[["tcga_mutation_plot"]] <- renderPlot(print(p_tcga_mutation))

  output[["tcga_segment_display"]] <- renderUI({
    shinycssloaders::withSpinner(plotOutput(paste0("tcga_segment_plot")))
  })
  output[["tcga_segment_plot"]] <- renderPlot(print(p_tcga_segment))

  output[["tcga_expression_display"]] <- renderUI({
    shinycssloaders::withSpinner(plotOutput(paste0("tcga_expression_plot")))
  })
  output[["tcga_expression_plot"]] <- renderPlot(print(p_tcga_expression))

  output[["tcga_methylation_display"]] <- renderUI({
    shinycssloaders::withSpinner(plotOutput(paste0("tcga_methylation_plot")))
  })
  output[["tcga_methylation_plot"]] <- renderPlot(print(p_tcga_methylation))
}
