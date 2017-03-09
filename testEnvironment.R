
library(httr)
library(jsonlite)
library(maps)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)
library(maps)



us.frame <-read.csv("data/Mar1-4.csv", stringsAsFactors = FALSE)

map.frame <- map_data("world") %>% 
  mutate(country = iso.alpha(region)) %>% 
  filter(country == "US") 

huh <- filter(us.frame, latitude > 25) %>% 
  filter(latitude < 55)









ggplot(data = map.frame, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group)) +
    coord_quickmap()


   


x <- map.cities(x = , country = "", label = TRUE,
          cex = par("cex"), projection = FALSE,
           parameters = NULL, orientation = NULL, pch = 1)





states <- map_data("state")
 

p <- ggplot() + 
  geom_polygon(data = states, fill = "#ffffff", color = "#000000", aes(x = long, y = lat, group = group))  + 
  geom_point(data = huh, aes(x = longitude, y = latitude, color = value))
p

