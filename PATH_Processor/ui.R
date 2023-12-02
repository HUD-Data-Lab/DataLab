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

ui <- page_sidebar(title = "PATH Annual Report Generator",
                    sidebar = dashboardSidebar(sidebarMenu(
                      menuItem("PATH", tabName = "path")
                    )),
                    body = dashboardBody(tabItems(
                      tabItem(tabName = "upload",
                              fluidPage(
                                fluidRow(
                                  box(
                                    solidHeader = TRUE,
                                    title = "Upload Hashed Data",
                                    fileInput("hashed_data", "Click below to upload a hashed CSV export.",
                                              accept = ".zip"),
                                    status = "warning",
                                    width = 12
                                  )
                                )#,
                                # fluidRow(
                                #   infoBoxOutput("arrest_time_range"),
                                #   infoBoxOutput("admit_time_range")
                                # ),
                                # fluidRow(
                                #   box(uiOutput("compare_to_date"), width = 12)
                                #   # infoBoxOutput("file_people", width = 6),
                                #   # infoBoxOutput("file_arrests", width = 6)
                                # )
                              )))),
                    theme = bs_theme(bootswatch = "spacelab",
                                     base_font = font_google("Open Sans")))

# ui <- page_sidebar(
#   title = "Penguins dashboard",
#   # sidebar = color_by,
#   theme = bs_theme(
#     bootswatch = "spacelab",
#     base_font = font_google("Open Sans")
#   )
# )
