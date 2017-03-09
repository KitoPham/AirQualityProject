
library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)
library(RColorBrewer)
library(plotly)

my.ui <- fluidPage(
  
  titlePanel(h1("Particle Matter 2.5 (PM2.5) Measurements in the United States")),
  p("This resource is a visualization display of locations in the United States that measure PM2.5, this resource enables one to view the various levels across different locations and compare them directly"),
  p("source: https://openaq.org"),
  sidebarLayout(
    sidebarPanel(
      uiOutput("selectthis"),
      uiOutput("selectthat"),
      sliderInput("date","Date:", 
                  min = as.Date("2017-02-07","%Y-%m-%d"),
                  max = as.Date("2017-03-07","%Y-%m-%d"),
                  value = c(as.Date("2016-03-01","%Y-%m-%d"),
                            as.Date("2017-03-07","%Y-%m-%d")),
                  timeFormat = "%Y-%m-%d")
      
    ),
    mainPanel(
      tabsetPanel(type="tabs",
        tabPanel(
          "Compare",
          plotlyOutput("overtime"),
          plotOutput("barCompare")
        ),
        
        tabPanel(
          "Map",
          plotOutput("plot", 
                     brush = brushOpts(id = "zoom_brush",resetOnNew = TRUE)
                     ),
          
          plotOutput("zoomed"),
          h6("Highlight an area on top graph to zoom")
          
        )
      )
    )
  )
)


