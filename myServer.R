library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)


my.server<-function(input,output){

us.frame <-read.csv("data/Mar1-4.csv", stringsAsFactors = FALSE)
states <- map_data("state")
ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  ##Plots World Map
  output$plot<-renderPlot({

    us.frame <- filter(us.frame, latitude > 25) %>% 
      filter(latitude < 55)
    
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#ffffff", color = "#000000", aes(x = long, y = lat, group = group))  + 
      geom_point(data = us.frame, aes(x = longitude, y = latitude, color = value))
  
  return(plot)
  })
  
#  PLACE
  
  ##Zoomed in Plot
  output$zoomed <- renderPlot({
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#ffffff", color = "#000000", aes(x = long, y = lat, group = group))  + 
      geom_point(data = us.frame, aes(x = longitude, y = latitude, color = value)) +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  
  return(plot)
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
  
  #set up for brush
  selecttable <- reactiveValues(
    row = data.frame()
  )
  #grabs selected points
  observeEvent(input$point_click, {
    selecttable$row <- nearPoints(us.frame, input$point_click, threshold = 10000, maxpoints = 1)
  })
  
  output$selected <- renderTable({
    text <- "Click on a point for city"
    if(!is.null(selecttable$row)){
      newtable <- selecttable$row
      if(nrow(selecttable$row) > 0){
        text <- newtable[1,]
      }
    }
    return(text)
  })
  
  output$selectedLocation <- renderTable({
    text <- "Click on a point for location"
    if(!is.null(selecttable$row)){
      newtable <- selecttable$row
      if(nrow(selecttable$row) > 0){
        text <- newtable[1,]
      }
    }
    return(text)
  })
  
  #output$overtime <- renderPlot({
 #   cityname <- selectedCity$city
  #  cityFrame <- filter(us.frame, city == cityname)
#  })
  
  
  
}



























