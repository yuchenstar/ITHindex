FROM openanalytics/r-ver:4.3.0
# https://hub.docker.com/r/openanalytics/r-ver/tags


# system libraries of general use
RUN apt-get update && apt-get install --no-install-recommends -y \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    && rm -rf /var/lib/apt/lists/*

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev \
    && rm -rf /var/lib/apt/lists/*

# basic shiny functionality
RUN R -q -e "options('repos' = c(CRAN='https://mirrors.ustc.edu.cn/CRAN/'))"
RUN R -q -e "install.packages('shiny')"
RUN R -q -e "install.packages('shinythemes')"
RUN R -q -e "install.packages('shinydashboard')"
RUN R -q -e "install.packages('shinyWidgets')"
RUN R -q -e "install.packages('shinyalert')"
RUN R -q -e "install.packages('shinybusy')"
RUN R -q -e "install.packages('shinydisconnect')"
RUN R -q -e "install.packages('shinycssloaders')"
RUN R -q -e "install.packages('shinyjs')"
RUN R -q -e "install.packages('rmarkdown')"
RUN R -q -e "install.packages('shinyvalidate')"
RUN R -q -e "install.packages('markdown')"
RUN R -q -e "install.packages('prompter')"
RUN R -q -e "install.packages('shinyBS')"
RUN R -q -e "install.packages('DT')"
RUN R -q -e "install.packages('mailtoR')"

# install dependencies of the devtools app
RUN R -q -e "install.packages('devtools')"
RUN R -q -e "devtools::install_github('AnalytixWare/ShinySky')"
RUN R -q -e "devtools::install_github('colearendt/shinycookie')"
RUN R -q -e "devtools::install_github('gadenbuie/shinyThings')"


RUN R -q -e "install.packages('remotes')"
RUN R -q -e "install.packages('future.apply')"
RUN R -q -e "install.packages('viridis')"
RUN R -q -e "install.packages('ggplot2')"
RUN R -q -e "install.packages('ggpubr')"
RUN R -q -e "install.packages('aplot')"
RUN R -q -e "install.packages('ggbreak')"
RUN R -q -e "remotes::install_github('paleolimbot/rbbt')"
#RUN R -q -e "remotes::install_github('YuLab-SMU/aplot')"
#RUN R -q -e "remotes::install_github('YuLab-SMU/ggbreak')"
#RUN R -q -e "install.packages('/opt/soft/aplot_0.0.6.tar.gz', repos = NULL, type = 'source')"
#RUN R -q -e "install.packages('/opt/soft/ggbreak_0.0.5.tar.gz', repos = NULL, type = 'source')"

RUN R -q -e "install.packages('naniar')"
RUN R -q -e "install.packages('openxlsx')"
RUN R -q -e "install.packages('vegan')"
RUN R -q -e "install.packages('spatstat')"
RUN R -q -e "install.packages('parallel')"
RUN R -q -e "install.packages('dplyr')"
RUN R -q -e "install.packages('magrittr')"
RUN R -q -e "install.packages('Hmisc')"
RUN R -q -e "install.packages('rintrojs')"
RUN R -q -e "install.packages('broom')"
RUN R -q -e "install.packages('tibble')"
RUN R -q -e "install.packages('tidyr')"
RUN R -q -e "install.packages('boot')"
RUN R -q -e "install.packages('MASS')"


RUN R -q -e "options(BioC_mirror='https://mirrors.ustc.edu.cn/bioc/')"
RUN R -q -e "install.packages('BiocManager')"
RUN R -q -e "BiocManager::install('limma')"
RUN R -q -e "BiocManager::install('BiocGenerics')"
RUN R -q -e "BiocManager::install('S4Vectors')"
RUN R -q -e "BiocManager::install('IRanges')"
RUN R -q -e "options(BIOCONDUCTOR_ONLINE_VERSION_DIAGNOSIS=T)"
RUN R -q -e "BiocManager::install('GenomeInfoDb')"
RUN R -q -e "BiocManager::install('GenomeInfoDbData')"
RUN R -q -e "BiocManager::install('GenomicRanges')"
RUN R -q -e "install.packages('gdata')"
RUN R -q -e "install.packages('squash')"
RUN R -q -e "install.packages('iotools')"
RUN R -q -e "devtools::install_github('zhanxw/seqminer')"


#RUN R -q -e "install.packages('/opt/soft/GenomeInfoDb_1.32.4.zip', repos = NULL, type = 'source')"
#RUN R -q -e "install.packages('/opt/soft/GenomeInfoDbData_1.2.8.tar.gz', repos = NULL, type = 'source')"

#RUN R -q -e "remotes::install_github('ShixiangWang/copynumber')"
RUN R -q -e "BiocManager::install('igordot/copynumber')"

#RUN R -q -e "install.packages('/opt/soft/sequenza_3.0.0.tar.gz', repos = NULL, type = 'source')"
RUN R -q -e "devtools::install_github('cran/sequenza')"


# copy the app to the image
RUN mkdir /root/ithindex023
COPY ITHindex /root/ithindex023

COPY Rprofile.site /usr/local/lib/R/etc/

EXPOSE 3843

CMD ["R", "-q", "-e", "shiny::runApp('/root/ithindex023')"]
