# import libraries

if(!require(tigris)) install.packages("tigris", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearth)) install.packages("rnaturalearth", repos = "http://cran.us.r-project.org")
if(!require(rnaturalearthdata)) install.packages("rnaturalearthdata", repos = "http://cran.us.r-project.org")

source("confirmed_case_data.R")

# Define server logic
shinyServer(function(input, output) {
  
  # get map data
  
  mapdata <- reactive({
    data <-
      geo_join(spatial_data, 
               case_data %>% filter(Date == input$mapdate), 
               'iso_a3', 'iso3')
    return(data)
  })
  
  # define base map
  
  output$globalmap <-
    renderLeaflet({
      leaflet(mapdata()) %>%
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
                      ~colorNumeric(colors, total_cases_range, n = 400, 
                                     reverse=TRUE)(total_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",geounit,"</strong></h4>",
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
                      ~colorQuantile(colors(400, rev = TRUE), daily_cases_range, n = 400, 
                                     reverse=TRUE)
                    (daily_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",Combined_Key,"</strong></h4>",
                                   "<p><strong>Total Cases: </strong>",total_cases %>% prettyNum(big.mark = ','),
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
                    fillColor = ~colorQuantile(colors, weekly_cases_range, n = 400, 
                                               reverse=TRUE)(weekly_pop),
                    fillOpacity = 0.6,
                    popup = ~paste("<h5>",Date,"</h5>",
                                   "<h4><strong>",Combined_Key,"</strong></h4>",
                                   "<p><strong>Total Cases: </strong>",total_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Daily Cases: </strong>",daily_cases %>% prettyNum(big.mark = ','),
                                   "</br><strong>New Weekly Cases: </strong>",weekly_cases %>% prettyNum(big.mark = ','),"</p>"))
    }
    
  })
  
  global_graph_data <- reactive({
    df <- case_data %>% filter(Combined_Key %in% top_countries$Combined_Key)
    return(df)
  })
  
  country_graph_data <- reactive({
    df <- case_data %>% filter(Combined_Key == input$country)
    df<- df %>% select(Date = Date, y = input$variable)
    return(df)
  })
  
  ylab <- reactive({
    if(input$variable == 'total_cases') {
      return("Total Cases")
    }
    if(input$variable == 'daily_cases') {
      return("New Cases (Daily)")
    }
    if(input$variable == 'weekly_cases') {
      return("New Cases (Weekly)")
    }
  })
  title <- reactive({
    t <- paste(ylab(), input$country, sep = ' in ')
  })
  f1 <- list(
    family = "Georgia, serif",
    size = 18,
    color = "white"
  )
  f2 <- list(
    family = "Georgia, serif",
    size = 18,
    color = "white"
  )
  f3 <- list(
    family = "Georgia, serif",
    size = 14,
    color = "white"
  )

  
  output$country_graph <- renderPlotly({
    plot_ly(data = country_graph_data(), 
            x = ~Date, 
            y = ~y, 
            type = 'scatter',
            mode = 'lines+markers',
            marker = list(size = 5,
                          color = "#FF6600"),
            line = list(width = 2,
                        color = "#FF6600")) %>%
        layout(title = list(text = title(),
                            font = f1),
               yaxis = list(title = ylab(), 
                            titlefont = f2,
                            tickfont = f3,
                            tickformat = ",d",
                            linecolor = "white",
                            gridcolor = "white"),
               xaxis = list(title = "Date",
                            titlefont = f1,
                            tickfont = f2,
                            linecolor = "white",
                            gridcolor = "white"),
               paper_bgcolor = "#1d1c1d",
               plot_bgcolor = "#1d1c1d")
    
  })
  
})


          