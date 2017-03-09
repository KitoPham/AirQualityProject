library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)
library(RColorBrewer)


my.server<-function(input,output){
  feb.7.10 <- read.csv('data/Feb7-10.csv', stringsAsFactors=FALSE)
  
  feb.11.15 <- read.csv('data/Feb11-15.csv', stringsAsFactors=FALSE)
  
  feb.16.20 <- read.csv('data/Feb16-20.csv', stringsAsFactors=FALSE)
  
  feb.21.24 <- read.csv('data/Feb21-24.csv', stringsAsFactors=FALSE)
  
  feb.25.26 <- read.csv('data/Feb25-26.csv', stringsAsFactors=FALSE)
  
  feb.27.28 <- read.csv('data/Feb27-28.csv', stringsAsFactors=FALSE)
  
  mar.1.4 <- read.csv('data/Mar1-4.csv', stringsAsFactors=FALSE)
  
  mar.5.7 <- read.csv('data/Mar5-7.csv', stringsAsFactors=FALSE)
  
  us.frame <- rbind(mar.5.7, mar.1.4, feb.27.28, feb.25.26,
                      feb.21.24, feb.16.20, feb.11.15, feb.7.10)
  
  us.frame <- aggregate(value~location + latitude + longitude + city, last.month, mean)
  
  us.frame <- filter(us.frame, latitude > 25) %>% 
    filter(latitude < 55)
  
  us.frame$value = cut(us.frame$value, 10, labels = c('0 to 3', "3 to 6", "6 to 8", "8 to 10", "10 to 13",
                                                      "13 to 16", "16 to 19", "19 to 22", "22 to 27", "NA"))

states <- map_data("state")
ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  ##Plots World Map
  output$plot<-renderPlot({

    us.frame <- filter(us.frame, latitude > 25) %>% 
      filter(latitude < 55)
    
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
    geom_point(data = us.frame, aes(x = longitude, y = latitude, color = value)) +
    scale_color_brewer(palette = 'Reds')
  
  return(plot)
  })
  
#  PLACE
  
  ##Zoomed in Plot
  output$zoomed <- renderPlot({
    
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
    geom_point(data = us.frame, aes(x = longitude, y = latitude, color = value)) +
    scale_color_brewer(palette = 'Reds') +
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



























