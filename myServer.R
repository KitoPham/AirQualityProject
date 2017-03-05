library(dplyr)
library(ggplot)
library(shiny)

rootUrl <- "https://api.openaq.org/v1/"
### response <- GET(url)
### body.data <- fromJSON(content(response,"text"))
### body.data <- flatten(body.data)

my.server<-function(input,output){
  #https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
  #append url with & date_to of slider input (use latest)
  
  
  #filter ggplot by country for world 
  
  
  #onclick change to country clicked
  
}