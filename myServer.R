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
  
  et.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","ET",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  
  
  es.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","ES",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  dk.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","DK",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
  })
  
  de.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","DE",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  cz.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","CZ",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  co.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","CO",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  cn.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","CN",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  cl.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","CL",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  ca.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","CA",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  br.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","BR",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  bh.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","BH",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  be.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","BE",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  bd.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","BD",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  ba.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","BA",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  au.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","AU",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  at.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","AT",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  fr.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","FR",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  gb.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","GB",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  gi.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","GI",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  hr.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","HR",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  hu.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","HU",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  id.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","ID",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  ie.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","IE",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  il.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","IL",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  in.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","IN",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  kw.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","KW",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  mk.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","MK",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  mn.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","MN",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  mx.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","MX",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })
  
  ng.frame <- reactive({
    dataUrl<-paste(urlParameter,input$parameter,"&limit=10000&country?=","NG",sep="")
    airData <- GET(dataUrl)
    airData <- content(airData,"text")
    airData<- fromJSON(airData)
    airData<- flatten(airData$results)
    airData <- group_by(data.frame(),country) %>% 
      summarize(value = mean(value))
  })

  output$plot<-renderPlot({
    
    airData <- map.frame
    airData <- left_join(airData,ar.frame)
    airData <- left_join(airData,at.frame)
    
    plot <- ggplot(data = airData, na.rm = TRUE) +
      geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
      coord_quickmap()
    return(plot)
  })
#onclick change to country clicked

}



























