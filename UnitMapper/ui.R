# Copyright (C) 2023 Gwen Beebe
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

fluidPage(
  theme = shinytheme("united"),
  titlePanel("Tucson Rental Units"),
  
  sidebarLayout(
    sidebarPanel = sidebarPanel(
      checkboxGroupInput(
        label = "Number Of Bedrooms",
        inputId = "bedrooms",
        # multiple = TRUE,
        choices = 
          # unique(apartment_table$bedrooms)
          c("Studio", "1 bedroom", "2 bedroom", "3 bedroom"),
        selected = c("Studio", "1 bedroom", "2 bedroom", "3 bedroom")
        ),
      sliderInput(
        label = "Rent Amount",
        inputId = "rent_amount",
        min = 0,
        max = max(apartment_table$price),
        value = c(0, max(apartment_table$price)))
      ),
    mainPanel = mainPanel(
      leafletOutput(outputId = 'map', height = "95vh")
      
    )
  )
  
)
