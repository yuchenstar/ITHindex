# ===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : ui.R
# @License : Copyright(C), X
# @Author  : wenchuan.xie
# @Time    : 2023-10-15
# @IDE     : RStudio
# @Desc    : UI
# ===============================================================================

# Shiny UI -------
ui <- fluidPage(

  shinyjs::useShinyjs(),
  shinycookie::initShinyCookie("cookies"),
  # use_tracking(),

  # cookie control
  # ref：https://github.com/DataScienceScotland/shiny_cookies
  # tags$script(src = "cookieControl-9.x.min.js"),
  # HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=G-S07KP4MCSN'></script>"),
  # tags$script(src = "cookie_control_config.js",style = 'fill:#00a064'),

  # session 过期样式
  disconnectMessage(
    text = "Your session has timed out!",
    refresh = "Reload now",
    background = "#646464e6",
    size = 30,
    width = 900,
    top = "center",
    colour = "white",
    overlayColour = "#999",
    overlayOpacity = 0.4,
    refreshColour = "#FFFFFF" # 00A064
  ),
  fluidRow(
    column(
    12,
    navbarPage(
      # 导航栏：左侧项目标题
      # title = p(strong("ITHindex"),
      #           style = "margin-top:0px;font-size:30px;color:#FFFFFF"),
      title = tags$img(
        src = "ITHindex.png",
        style = "margin-top:-5px;margin-right:-30px;width:100px;height:35px;"
      ),
      id = "titlebar",
      # 固定标题栏不随滚动条滚动
      position = "fixed-top",
      includeCSS("www/ithindex.css"),
      theme = shinytheme("cosmo"),
      collapsible = TRUE, # 菜单可折叠
      ###########################################################################
      ## 首页
      tabPanel(
        title = strong("Home"),
        icon = icon("house"),
        mainPanel(
          width = 11,
          style = "margin-top:3%;",
          rintrojs::introBox(
            fluidRow(
              column(7, p(h3("", strong("ITHindex"), ""),style = "color:black;")), # 给一个空行
              column(
                12,
                includeMarkdown("rmd/home.Rmd"),
                style = "font-size:20px;",
                hr(style = "border-color: #cbcbcb;")
              ),
              # Content
              column(7,
                align = "center",
                rintrojs::introBox(
                  fluidRow(
                    tags$img(
                      src = "GraphicAbstract_3.0.jpg",
                      style = "margin-left:5%;margin-right:5%;margin-bottom:5px;margin-top:15px;width:100%;"
                    )
                  ),
                  data.position = "bottom-middle-aligned"
                )

              ),
              column(5,
                align = "left",
                style = "margin-bottom:10%;font-size:20px;",
                p(strong("How does ITHindex work?"), style = "margin-left:20%;font-size:30px;"),
                p(HTML("&bull;"),
                  "STEP1: Choose the data type you want to use!",
                  style = "margin-left:20%;"
                ),
                p(HTML("&bull;"),
                  "STEP2: Choose an algorithm to quantify ITH!",
                  style = "margin-left:20%"
                ),
                p(HTML("&bull;"),
                  "STEP3: Upload your data file!",
                  style = "margin-left:20%"
                ),
                p(HTML("&bull;"),
                  "STEP4: View the ITH index!",
                  style = "margin-left:20%"
                ),
                br(),
                div(
                  # style of button
                  tags$head(tags$style(HTML('#start_now{background-color:#111;border-color:#111}'))),
                  style = "margin-left:20%;float:left;", # 圆角：display:inline-block;
                  actionButton(
                    inputId = "start_now",
                    label = strong("Try it Now!", style = "font-size:25px;"),
                    class = "btn-primary",
                    style = "success"
                  )
                )
              ),
              br(),
            ),
            data.step = 1,
            data.intro = (p(h3("ITHindex"))),
            data.position = "left"
          ),
        )
      ),

      ###########################################################################
      ## 右侧功能标题
      tabPanel(strong("ITH Index"),
        value = "ithindex", icon = icon("magnifying-glass-chart"),
        ith_quantification_page$ui),
      ## TCGA data
      tabPanel(strong("Data"),
               value = "Data", icon = icon("table"),
               ith_data_page$ui),
      ## About
      tabPanel(strong("About"),
               value = "about", icon = icon("question"),
               ith_about_page$ui)
    ),
    # 脚注 ----
    tags$footer(
      wellPanel(
        style = 'border:#ffffff;background:#111;position:relative;',
        fluidRow(
          column(8,
            align = "center",
            div(style = "height:10px;"), # 插入10px空行
            p(
              tags$a(
                href="http://www.cams.ac.cn/",target = "_blank",
                tags$img(src="xiehe_long.png",
                         title="Chinese Academy of Medical Sciences & Peking Union Medical College",
                         width="270",
                         height="100")),
              tags$a(
                href="https://www.dgphospital.com/",target = "_blank",
                tags$img(src="dongguan_long.png",
                         title="The Tenth Affiliated Hospital of Southern Medical University",
                         width="360",
                         height="100")),
              tags$a(
                href="https://www.brbiotech.com/",target = "_blank",
                tags$img(src="bnr.png",
                         title="Burning Rock Dx",
                         width="140",
                         height="100"))
            ),
            p("Copyright © 2024 ",
              # tags$a(href = "http://www.cams.ac.cn/", "Chinese Academy of Medical Sciences & Peking Union Medical College", target = "_blank"),
              # ", ",
              # tags$a(href = "https://www.dgphospital.com/", "The Tenth Affiliated Hospital of Southern Medical University", target = "_blank"),
              # " and ",
              # tags$a(href = "https://www.brbiotech.com/", "Burning Rock Dx", target = "_blank"),
              ". All Rights Reserved.",
              style = "font-size:120%;color:white;"
            ),
            p(tags$em("Version: 0.2.1, Last updated: Aug 2024"),
              style = "font-size:120%;color:white;")
          ),
          ## 新增用户统计
          column(4,
            align = "center",
            # p(HTML('<div style="display:inline-block;width:200px;"><script type="text/javascript" src="//rf.revolvermaps.com/0/0/7.js?i=5elhn40z8tw&amp;m=7&amp;c=ff0000&amp;cr1=ffffff&amp;sx=0" async="async"></script></div>'))
            p(HTML('<div style="display:inline-block;width:200px;"><script type="text/javascript" id="clustrmaps" src="//clustrmaps.com/map_v2.js?d=WvJST7ZP0BTcpRxwrnCJZJL_uT8r5lpfMGAAcn1Ob5Y&cl=ffffff&w=a"></script></div>'))
            # p(HTML('<div style="display:inline-block;width:200px;"><script type="text/javascript" id="clstr_globe" src="//clustrmaps.com/globe.js?d=WvJST7ZP0BTcpRxwrnCJZJL_uT8r5lpfMGAAcn1Ob5Y"></script></div>'))
          )
        )
      )
    )
    # footer end
  )
  )
)
