library(maps)


library(maps)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
#https://api.openaq.org/v1/measurements?parameter= (ui.dropdown input, ex= pm25)
urlHeader <- "https://api.openaq.org/v1/measurements"
urlParameter<-urlHeader + "?parameter="
#append url with & date_to of slider input (use latest)

dataUrl <- "https://api.openaq.org/v1/measurements?parameter=pm25&date_to=2015-12-06"
airData <- GET(dataUrl)
airData <- content(airData,"text")
airData<- fromJSON(airData)
airData<- flatten(airData$results)

airData<- group_by(airData,country) %>% 
            summarize(value = mean(value))



map.frame <- map_data("world") %>% 
  mutate(country = iso.alpha(region))
airData <- left_join(map.frame,airData)

ggplot(data = airData, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
    coord_quickmap()

   