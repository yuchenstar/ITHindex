#===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : server.R
# @License : Copyright(C), X
# @Author  : wenchuan.xie
# @Time    : 2023-10-15
# @IDE     : RStudio
# @Desc    : Server
#===============================================================================


# Shiny Server Side -------
server <- function(input, output, session) {

  # Click the btn and go to ITHindex tabPanel!
  observeEvent(input$start_now, {
    updateNavbarPage(
      session = getDefaultReactiveDomain(),
      inputId = 'titlebar',
      selected = 'ithindex'
    )
  })

  ###########################################################################
  # 每个功能 tab-server
  ith_quantification_page$server(input, output, session)
  ith_data_page$server(input, output, session)
  ith_about_page$server(input, output, session)



}
