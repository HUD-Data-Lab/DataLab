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



# ui <- page_sidebar(title = "PATH Annual Report Generator",
#                     sidebar = dashboardSidebar(sidebarMenu(
#                       menuItem("PATH", tabName = "path")
#                     )),
#                     body = dashboardBody(tabItems(
#                       tabItem(tabName = "upload",
#                               fluidPage(
#                                 fluidRow(
#                                   box(
#                                     solidHeader = TRUE,
#                                     title = "Upload Hashed Data",
#                                     fileInput("hashed_data", "Click below to upload a hashed CSV export.",
#                                               accept = ".zip"),
#                                     status = "warning",
#                                     width = 12
#                                   )
#                                 )#,
#                                 # fluidRow(
#                                 #   infoBoxOutput("arrest_time_range"),
#                                 #   infoBoxOutput("admit_time_range")
#                                 # ),
#                                 # fluidRow(
#                                 #   box(uiOutput("compare_to_date"), width = 12)
#                                 #   # infoBoxOutput("file_people", width = 6),
#                                 #   # infoBoxOutput("file_arrests", width = 6)
#                                 # )
#                               )))),
#                     theme = bs_theme(bootswatch = "spacelab",
#                                      base_font = font_google("Open Sans")))




ui <- 
  fluidPage(
    # thematic_shiny(),
    theme = ICF_theme,
    
    ICF_TitlePanel("PATH Report Generator"),
    
    # fluidRow(
    #     h1("First level title"),
    #     h2("Second level title"),
    #     h3("Third level title"),
    #     h4("Fourth level title"),
    #     h5("Fifth level title"),
    #     h6("Sixth level title"),
    #     box(
    #       "test primary", status = "primary"
    #     ),
    #     box(
    #       "test success", status = "success"
    #     ),
    #   ),
    fluidRow(
      card(card_header(
        class = "bg-danger",
        "Instructions"),
        HTML("<p>To generate a <a href='https://www.hudexchange.info/resource/5212/final-hmis-programming-specifications-path-annual-report/'
              target= '_blank' rel='noopener noreferrer'>PATH report</a>, you will need a hashed 
              <a href='https://files.hudexchange.info/resources/documents/HMIS-CSV-Format-Specifications-2024.pdf'
              target= '_blank' rel='noopener noreferrer'>HMIS CSV Export</a>.
              </p>
              <p>Generate a hashed HMIS CSV Export from your local HMIS and store
              it in a secure location that you can easily find again. It must be
              a .zip file with 23 csv files in it.
              <ul>
              <li>A hashed export means that the personal identifiers are obscured
              when the export is generated.</li>
              <li>The HMIS CSV Export has client-level data in it, so it must be
              stored in a secure location per HUD, state, and local rules and
              regulations.</li>
              <li>If you are unsure how to generate your hashed HMIS CSV Export,
              please contact your vendor.</li>
              </ul>
              Once you have exported the correct file from your HMIS, you are
              ready to generate a PATH report. Begin by uploading your zip file below."))
    ),
    fluidRow(
      card(
        fileInput("imported",
                  label = NULL,
                  multiple = FALSE,
                  accept = ".zip")#,
        # uiOutput("fileInfo")
      )),
    fluidRow(
      card(
        uiOutput("org_selector")
      ),
      card(
        uiOutput("report_start_date"),
        uiOutput("report_end_date"))),
    # fluidRow(
    #   card(
    #     DT::dataTableOutput("pdde_summary_table")
    #   )
    # ),
    fluidRow(downloadButton('downloadall', 'Download PATH Report')),
    fluidRow(
      card(card_header(
        "Citations"),
        HTML("<p>This project would not exist were it not for the existence of
         other quality, free and open source products, particularly the following:
         
         <p>The wonderful <a href = 'https://github.com/abtassociates/eva' target= '_blank' rel='noopener noreferrer'>Eva</a> app developed by Abt 
         
         <p>The <a href = 'https://cran.r-project.org/package=tidyverse' 
         target= '_blank' rel='noopener noreferrer'>tidyverse</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=stringr' 
         target= '_blank' rel='noopener noreferrer'>stringr</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=janitor' 
         target= '_blank' rel='noopener noreferrer'>janitor</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=shiny' 
         target= '_blank' rel='noopener noreferrer'>shiny</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=bslib' 
         target= '_blank' rel='noopener noreferrer'>bslib</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=DT' 
         target= '_blank' rel='noopener noreferrer'>DT</a> package
         
         <p>The <a href = 'https://cran.r-project.org/package=zip' 
         target= '_blank' rel='noopener noreferrer'>zip</a> package")))
    
  )
# 
# theme <- bslib::bs_theme(
#   bg = "#0b3d91",
#   fg = "white",
#   base_font = font_google("Open Sans")
# )
# 
# bslib::bs_theme_preview(theme)
# 

