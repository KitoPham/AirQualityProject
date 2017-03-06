library(dplyr)
library(ggplot)
library(shiny)
library(httr)
library(jsonlite)

rootUrl <- "https://api.openaq.org/v1/"
### response <- GET(url)
### body.data <- fromJSON(content(response,"text"))
### body.data <- flatten(body.data)

my.server<-function(input,output){
  #https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
  urlHeader <- "https://api.openaq.org/v1/measurements"
  urlParameter<-urlHeader + "?parameter="
  #append url with & date_to of slider input (use latest)
  dateInput <- reactive{
    return (input$date)
  }
  dataUrl <- urlParameter + dateInput()
  airData <- GET(dataUrl)
  airData <- content(airData,"text")
  airData<- fromJSON(airData)
  airData<- flatten(airData$results)
  
  
  #filter ggplot by country for world
  output$plot <- renderPlot(
    map.frame = map_data("world")
    plot <- ggplot(data = map.frame, na.rm = TRUE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = GDP)) +
      scale_fill_brewer(palette = "YlOrRd") +
      coord_quickmap()
  )
  
  #onclick change to country clicked
  
}