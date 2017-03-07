
library(httr)
library(jsonlite)
library(maps)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)

#https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
urlHeader <- "https://api.openaq.org/v1/measurements"
urlParameter<-urlHeader + "?parameter="
#append url with & date_to of slider input (use latest)

dataUrl <- "https://api.openaq.org/v1/measurements?parameter=pm10&limit=10000&order_by=date&sort=desc"
airData <- GET(dataUrl)
airData <- content(airData,"text")
airData<- fromJSON(airData)
airData<- flatten(airData$results)

filterDate<- airData %>% 
  filter(date.utc > input$date[1]) %>% 
  filter(date.utc < input$date[2])

airData<- group_by(airData,country) %>% 
            summarize(value = mean(value))




map.frame <- map_data("br") %>% 
  mutate(country = iso.alpha(region))
airData <- left_join(map.frame,airData)

ggplot(data = airData, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
    coord_quickmap()

   