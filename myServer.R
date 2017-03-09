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
  
  us.all.frame <- rbind(mar.5.7, mar.1.4, feb.27.28, feb.25.26,
                      feb.21.24, feb.16.20, feb.11.15, feb.7.10)
  us.reactive <- reactive({
    frame <- filter(us.all.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
              filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z")))
    frame <- aggregate(value~location + latitude + longitude + city, frame, mean)
    
    frame <- filter(frame, latitude > 25) %>% 
      filter(latitude < 55)
    
    #frame$value = cut(frame$value, 10, labels = c('0 to 3', "3 to 6", "6 to 8", "8 to 10", "10 to 13","13 to 16", "16 to 19", "19 to 22", "22 to 27", "NA"))
    return(frame)
  })
  
  output$selectthis <- renderUI({
    selectInput("city1", "City 1", unique(us.all.frame$city))
  }) 
  output$selectthat <- renderUI({
    selectInput("city2", "City 2", unique(us.all.frame$city))
  })
  #updateSelectInput(session, "selectthis", choices = unique(us.frame$location))
  #updateSelectInput(session, "selectthat", choices = unique(us.frame$location))
    
                                                                                                 
  

states <- map_data("state")
ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  ##Plots World Map
  output$plot<-renderPlot({
    
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
    geom_point(data = us.reactive(), aes(x = longitude, y = latitude, color = value, size = value)) +
    scale_color_gradient(low = "White", high = "red")
  
  return(plot)
  })
  
#  PLACE
  
  ##Zoomed in Plot
  output$zoomed <- renderPlot({
    
  plot <- ggplot() + 
      geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
      geom_point(data = us.reactive(), aes(x = longitude, y = latitude, color = value, size = value)) +
    scale_color_gradient(low = "White", high = "red") +
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
  
  output$overtime <- renderPlot({
    if (!is.null(input$city1)&&!is.null(input$city2)){
      city1.frame <- filter(us.all.frame, city == input$city1);
      city1.frame <- filter(city1.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
                      filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z")))
      
      city2.frame <- filter(us.all.frame, city == input$city2);
      city2.frame <- filter(city2.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
        filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z")))
      
      plot <- ggplot() +
        geom_point(data = city1.frame, mapping = aes(x = utc, y = value), color = "RED")+
        geom_smooth(color = "RED")+
        geom_point(data = city2.frame, mapping = aes(x = utc, y = value), color = "BLUE")+
        geom_smooth(color = "BLUE")
      return(plot)
    }
  })
  
  output$barCompare <- renderPlot({
    if (!is.null(input$city1)&&!is.null(input$city2)){
      city1.frame <- filter(us.reactive(), city == input$city1) %>% 
                      group_by(city) %>% 
                        summarize(average = mean(value))
      city2.frame <- filter(us.reactive(), city == input$city2)%>% 
                      group_by(city) %>% 
                        summarize(average = mean(value))
      cityframe <- rbind(city1.frame, city2.frame)
      
      plot <- ggplot() +
        geom_col(data = cityframe, mapping = aes(x = city, y = average, fill=city))
      
      return(plot)
    }
  })
  
  
  
}



























