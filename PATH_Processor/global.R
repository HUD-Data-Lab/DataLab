# Copyright (C) 2023 ICF
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published
# by the Free Software Foundation, either version 3 of the License, or
# any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
# GNU Affero General Public License for more details at
# <https://www.gnu.org/licenses/>. 

library(tidyverse)
library(stringr)
library(janitor)
library(shiny)
library(bslib)
library(thematic)
library(DT)
library(zip)
# library(shinydashboard)
# library(bsicons)

source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/datalab_functions.R")
source("https://raw.githubusercontent.com/HUD-Data-Lab/DataLab/main/DataLab_Lists.R")


# source("https://raw.githubusercontent.com/abtassociates/eva/main/helper_functions.R")
logSessionData <- function() {return()}
logMetadata <- function() {return()}
evachecks <- read_csv("https://raw.githubusercontent.com/abtassociates/eva/9d4ba5c637134e7ebc72cfd337891feffe8b2a6b/public-resources/EvaChecks.csv",
                      show_col_types = FALSE)
cols_and_data_types <- read_csv("https://raw.githubusercontent.com/abtassociates/eva/main/public-resources/columns.csv", 
                                col_types = cols()) %>%
  filter(!(File %in% c("Affiliation",
                       "AssessmentResults",
                       "AssessmentQuestions")))
data_type_mapping <- c(
  character = "c", 
  numeric = "n", 
  date = "D",
  datetime = "T"
)
get_col_types <- function(file) {
  # get the column data types expected for the given file
  col_types <- cols_and_data_types %>%
    filter(File == file) %>%
    mutate(DataType = data_type_mapping[as.character(DataType)]) %>%
    pull(DataType) %>%
    paste0(collapse = "")
  return(col_types)
}

ICF_theme <- bslib::bs_theme(
  bg = "#ffffff", fg = "#000000", primary = "#0785F2",
  secondary = "#031D40", success = "#30F298", info = "#5BCBF5",
  warning = "#FFC628", danger = "#414042", base_font = font_google("DM Sans"),
  code_font = font_google("DM Mono"), heading_font = "DM Sans Black",
  `enable-shadows` = TRUE
  , preset = "spacelab"
)

ICF_TitlePanel <- function (title, windowTitle = title, color = "#031D40") {
  css <- paste(paste0("background-color:", color),
               "color: white",
               # "margin-left: 15px",
               "margin-top: -15px",
               "margin-left: -30px",
               "margin-right: -12px",
               "border-radius: 25px",
               "padding-top: 17px",
               "padding-left: 25px",
               "padding-bottom: 5px",
               sep = ";")
  tagList(tags$head(tags$title(windowTitle)), 
          h1(title, style = css))
}


