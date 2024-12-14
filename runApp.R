# ===============================================================================
# !/user/bin/env RStudio
# -*- coding: utf-8 -*-
# @File    : runApp.R
# @License : Copyright(C), X
# @Author  : Wenchuan Xie
# @Time    : 2024-01-10
# @IDE     : RStudio
# @Desc    : ITHindex start
# ===============================================================================

rm(list = ls())
gc()

library(shiny)
runApp("../ITHindex/",launch.browser = T)



