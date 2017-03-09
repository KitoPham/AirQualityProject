library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)
library(maps)
library(tidyr)

rootUrl <- "https://api.openaq.org/v1/"
### response <- GET(url)
### body.data <- fromJSON(content(response,"text"))
### body.data <- flatten(body.data)

my.server<-function(input,output){
  #"https://api.openaq.org/v1/measurements?parameter=" (ui.dropdown input, ex= pm25)
  
  map.frame <- map_data("US") %>% 
    mutate(country = iso.alpha(region))
  #urlHeader <- "https://api.openaq.org/v1/measurements"
  
  #urlParameter <- paste(urlHeader,"?parameter=",sep="")
  #append url with & date_to of slider input (use latest)
  

  ###Country Frame SetUp #############
  #ar.frame <- reactive({
    dataUrl<-paste("https://api.openaq.org/v1/measurements?parameter=",input$parameter,"&limit=10000&country?=","US",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData <- fromJSON(airData)
    airData <- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))

  })
  
  ###End of country frames set up
  
  
  ################# World Plot Render ######################
  output$plot<-renderPlot({
    
    ###grabs the average values and assigns to country
    ###################################################
    
    airData <- map.frame
    
    ###airData <- left_join(airData,ar.frame())
    
  
    
    ###End of Map Setup

    ################
    plot <- ggplot(data = airData, na.rm = TRUE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
      coord_quickmap()
    return(plot)
  })
#onclick change to country clicked
}



























