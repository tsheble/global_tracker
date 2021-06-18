if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(plotly)) install.packages("plotly", repos = "http://cran.us.r-project.org")
source("confirmed_case_data.R")

# Define UI for application 

shinyUI(
  bootstrapPage(
    fluidPage(
      tabsetPanel(
        
        # map tab panel
        tabPanel("COVID-19 Global Map",
           div(class = "outer",
               tags$head(includeCSS("styles.css")),
               absolutePanel(id = 'themap', top = 100, left = 225, width = 800, height = 600, class = 'pannel panel-default',
                             leafletOutput("globalmap", width = 800, height = 600)),
               absolutePanel(id = 'mapcontrols', class = 'pannel panel-default',
                             top = 100, left = 25, width = 175, fixed = FALSE,
                             draggable = FALSE,
                             radioButtons('mapvariable',
                                          label = 'Map:',
                                          choiceNames = c("Total Cases",
                                                          "New Daily Cases",
                                                          "New Weekly Cases"),
                                          choiceValues = c("total_pop",
                                                           "daily_pop",
                                                           "weekly_pop")),
                             dateInput("mapdate",
                                       "Date: ",
                                       value = today(tzone = "US/Pacific")-days(1))))
        ),
        
        # graph tab panel
        tabPanel("COVID-19 Global Cases",
           div(class = "outer",
               tags$head(includeCSS("styles.css")),
               absolutePanel(id = 'graphcontrols', class = 'pannel panel-default',
                             top = 100, left = 55, width = 175, height = 375,
                             fixed = FALSE, draggable = FALSE,
                             radioButtons('variable',
                                          label = 'Controls:',
                                          choiceNames = c("Total Cases",
                                                          "New Daily Cases",
                                                          "New Weekly Cases"),
                                          choiceValues = c("total_cases",
                                                           "daily_cases",
                                                           "weekly_cases")),
                             selectizeInput('country',
                                            label = 'Country:',
                                            choices = top_countries)),
               absolutePanel(id = 'countrygraph', class = 'panel panel-default',
                             top = 100, left = 285, width = 600, height = 375,
                             plotlyOutput("country_graph", width = '100%', height = '100%')))
        )
      )
    )
  )
)

