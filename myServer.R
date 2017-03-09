library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)


my.server<-function(input,output){
  
  map.frame <- map_data("US") %>% 
    mutate(country = iso.alpha(region))
  
  output$plot<-renderPlot({
    
    airData <- map.frame
    
    plot <- ggplot(data = airData, na.rm = TRUE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
      coord_quickmap()
    return(plot)
  })
#onclick change to country clicked
  
}



























