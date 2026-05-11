# home_upper_tab_ui.R

home_upper_tab_ui <- tagList(
  tags$head(
    tags$style(HTML("
      .home-container { background: #ffffff; padding: 30px 0; font-family: 'Inter', sans-serif; }
      .hero-wrap { text-align: center; margin-bottom: 40px; }
      .hero-title {
        font-size: 44px; font-weight: 900; color: #1a2a6c;
        background: linear-gradient(to right, #1a2a6c, #b21f1f, #fdbb2d);
        -webkit-background-clip: text; -webkit-text-fill-color: transparent;
        margin-bottom: 15px;
      }
      .hero-desc {
        font-size: 18px; color: #576574; max-width: 850px;
        margin: 0 auto; line-height: 1.6; font-weight: 400;
      }

      /* 卡片容器：设置为相对定位，以便图标绝对定位到右上角 */
      .feature-card {
        background: #ffffff; border: 1px solid #f0f0f0; border-radius: 12px;
        padding: 30px 25px; transition: all 0.4s ease;
        position: relative; height: 100%; top: 0;
        overflow: hidden;
      }
      .feature-card:hover {
        top: -5px; border-color: #3498db;
        box-shadow: 0 10px 25px rgba(0,0,0,0.05);
      }

      /* 图标框：绝对定位到右上角，缩小尺寸 */
      .icon-box {
        position: absolute;
        top: 15px;
        right: 15px;
        width: 40px; height: 40px; border-radius: 8px;
        display: flex; align-items: center; justify-content: center;
        font-size: 18px;
        opacity: 0.8;
      }
      .ib-blue { background: #e3f2fd; color: #1976d2; }
      .ib-green { background: #e8f5e9; color: #2e7d32; }
      .ib-amber { background: #fff8e1; color: #ffa000; }

      .card-h { font-size: 20px; font-weight: 700; color: #2d3436; margin-bottom: 12px; padding-right: 45px; }
      .card-p { font-size: 18px; color: #636e72; line-height: 1.6; }

      /* 修复底部间距过宽的问题 */
      .home-hr { border-color: #f1f2f6; margin-top: 20px; margin-bottom: 10px; }
    "))
  ),

  div(class = "home-container",
      fluidRow(
        column(4, div(class = "feature-card",
                      div(class = "icon-box ib-blue", icon("network-wired")), # 图标现在会自动移动到右上角
                      div(class = "card-h", "ITHindex"),
                      div(class = "card-text card-p", "ITHindex is an interactive web-based platform designed for the systematic quantification of ITH by integrating diverse algorithmic frameworks across genomic, transcriptomic, and epigenetic dimensions.")
        )),
        column(4, div(class = "feature-card",
                      div(class = "icon-box ib-green", icon("dna")),
                      div(class = "card-h", "Research Enabling"),
                      div(class = "card-text card-p", "By providing a unified interface to calculate multi-dimensional ITH scores — encompassing genetic, transcriptomic, proteomic, and epigenetic heterogeneity — ITHindex serves as a research-enabling tool to assist researchers in exploring the mechanisms of tumor progression and metastasis.")
        )),
        column(4, div(class = "feature-card",
                      div(class = "icon-box ib-amber", icon("shield-virus")),
                      div(class = "card-h", "Limitations"),
                      div(class = "card-text card-p", "As a downstream analytical calculator, ITHindex is inherently sensitive to the quality of input data; therefore, rigorous upstream quality control—particularly in variant calling and sequencing noise reduction—is essential to ensure the reliability of the derived ITH metrics.")
        ))
      )
  ),
  hr(class = "home-hr")
)
