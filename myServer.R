library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)


my.server<-function(input,output){

  ##Plots World Map
  output$plot<-renderPlot({
    
  })
  
  PLACE
  
  ##Zoomed in Plot
  output$zoom <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  })
  
  ##Check for zoom range
  observe({
    brush <- input$zoom_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  selectedCity <- reactive({
    nearPoints(placeHolder, input$plot_click, addDist = TRUE, maxpoints = 1)
  })
  
  output$overtime <- renderPlot({
    cityname <- selectedCity$city
    cityFrame <- filter(placeHolder, city == cityname)
  })
  
  
  
}



























