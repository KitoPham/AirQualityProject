library(dplyr)
library(ggplot2)
library(shiny)
library(httr)
library(jsonlite)

rootUrl <- "https://api.openaq.org/v1/"
### response <- GET(url)
### body.data <- fromJSON(content(response,"text"))
### body.data <- flatten(body.data)

my.server<-function(input,output){
  #https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
  map.frame <- map_data("world") %>% 
    mutate(country = iso.alpha(region))
  urlHeader <- "https://api.openaq.org/v1/measurements"
  urlParameter<-paste(urlHeader,"?parameter=",sep="")
  #append url with & date_to of slider input (use latest)
  
  ar.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","US",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  nl.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","NL",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  no.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","NO",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  np.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","NP",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  pe.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","PE",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  ph.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","ph",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  us.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","US",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
  })
  pl.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","PL",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  se.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","SE",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  sg.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","SG",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  th.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","TH",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  tr.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","TR",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  tw.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","TW",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  ug.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","UG",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  vn.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","VN",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  XK.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","xk",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  output$plot<-renderPlot({
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
    
    mapCopy <- map.frame
    airData <- left_join(mapCopy,airData)
  
    plot <- ggplot(data = airData, na.rm = TRUE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
      coord_quickmap()
    return(plot)
  })
  #onclick change to country clicked
  
}

