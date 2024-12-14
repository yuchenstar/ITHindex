#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

rm(list = ls())
gc()

# renv::snapshot()
library(shiny)
# library(ITHindex)
shinyAppDir("../ITHindex/",options = list())
# shinyuieditor::launch_editor(app_loc = "../ITHshiny/")
