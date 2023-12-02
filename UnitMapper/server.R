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

function(input, output, session) {


  
  # m <- leaflet(complexes_table) %>% 
  #   addProviderTiles(providers$Thunderforest.Transport,
  #                    options = providerTileOptions(
  #                      apikey = thunderforest_key
  #                    )) %>% 
  #   # setView(-71.931180, 42.385453, zoom = 7) %>% 
  #   addCircles(~lng, ~lat, 
  #              popup = complexes_table$name,
  #              weight = 3, radius=40, 
  #              color="#ffa500", stroke = TRUE, fillOpacity = 0.8) 

  
  map_df = reactive({
    
    apartment_table %>%
      filter(bedrooms %in% input$bedrooms &
               price >= input$rent_amount[1] &
               price <= input$rent_amount[2]) %>%
      inner_join(complexes_table,
                 by = c("complex" = "airtable_record_id"))
    
  })
  
  output$map = renderLeaflet({
    
    # leaflet() %>%
    #   addTiles() %>%
    #   addCircleMarkers(data = map_df(), radius = 40)
    
    leaflet(map_df()) %>%
      addProviderTiles(providers$Thunderforest.Transport,
                       options = providerTileOptions(
                         apikey = thunderforest_key
                       )) %>%
      # setView(-71.931180, 42.385453, zoom = 7) %>%
      addCircles(~lng, ~lat,
                 popup = map_df()$name,
                 weight = 3, radius=150,
                 color="navy", stroke = TRUE, fillOpacity = 0.8)
    
  })  
  
}
