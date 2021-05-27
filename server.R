#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
if(!require(tigris)) install.packages("tigris", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")

library(shiny)
source("confirmed_case_data.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  # map data
  mapdata <- reactive({
    data <-
      geo_join(spatial_data, 
               case_data %>% filter(Date == input$dateinput), 
               'iso_a3', 'iso3')
    return(data)
  })
  
  # base map
    
  output$globalmap <-
      renderLeaflet({
        leaflet(map_data) %>%
          addTiles(map_url) %>%
          fitBounds(lat1 = -10, lng1 = -90, lat2 = 10, lng2 = 90)
      })
  
  observe({
    if(input$mapvariable == "total_pop") {
      
      leafletProxy(mapId = "globalmap", data = mapdata()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = mapdata(), color = 'white', weight = 0.5,
                    fillColor = 
                      ~colorQuantile(colors, total_cases_range, n = 400, 
                                     reverse=TRUE)
                    (total_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",Combined_Key,"</strong></h4>",
                                   "<p><strong>Total Cases: </strong>",total_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Daily Cases: </strong>",daily_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Weekly Cases: </strong>",weekly_cases %>% prettyNum(big.mark = ','),"</p>"))
    }
    
  })
  
  observe({
    if(input$mapvariable == "daily_pop") {
      
      leafletProxy(mapId = "globalmap", data = mapdata()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = mapdata(), color = 'white', weight = 0.5,
                    fillColor = 
                      ~colorQuantile(colors, daily_cases_range, n = 400, 
                                     reverse=TRUE)
                    (daily_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",Combined_Key,"</strong></h4>",
                                   "<p><strong>Total Cases: </strong>",total_pop %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Daily Cases: </strong>",daily_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Weekly Cases: </strong>",weekly_cases %>% prettyNum(big.mark = ','),"</p>"))
    }
    
  })  
  
  observe({
    if(input$mapvariable == "weekly_pop") {
      
      leafletProxy(mapId = "globalmap", data = mapdata()) %>%
        clearShapes() %>%
        clearControls() %>%
        addPolygons(data = mapdata(), color = 'white', weight = 0.5,
                    fillColor = 
                      ~colorQuantile(colors, daily_cases_range, n = 400, 
                                     reverse=TRUE)
                    (daily_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",Combined_Key,"</strong></h4>",
                                   "<p><strong>Total Cases: </strong>",total_pop %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Daily Cases: </strong>",daily_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Weekly Cases: </strong>",weekly_cases %>% prettyNum(big.mark = ','),"</p>"))
    }
    
  })
  
})


