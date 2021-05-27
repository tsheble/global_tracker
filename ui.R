# setwd("~/R_projects/global_covid/covid_global_tracker")

if(!require(leaflet)) install.packages("leaflet", repos = "http://cran.us.r-project.org")
if(!require(shiny)) install.packages("shiny", repos = "http://cran.us.r-project.org")
if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")

# Define UI for application that draws a histogram

shinyUI(
  bootstrapPage(
    fluidPage(
      tabsetPanel(
        tabPanel("COVID-19 Tracker",
           div(class = "outer",
               tags$head(includeCSS("styles.css")),
               leafletOutput("globalmap", width = "100%", height = 1000),
               absolutePanel(id = 'mapcontrols', class = 'pannel panel-default',
                             top = 100, left = 55, width = 175, fixed = FALSE,
                             draggable = TRUE,
                             radioButtons('mapvariable',
                                          label = 'Map:',
                                          choiceNames = c("Cases",
                                                          "New Daily Cases",
                                                          "New Weekly Cases"),
                                          choiceValues = c("total_pop",
                                                           "daily_pop",
                                                           "weekly_pop")),
                             dateInput("dateinput",
                                       "Date: ",
                                       value = today(tzone = "US/Pacific")-days(1))))
    
        )
      )
    )
  )
)
