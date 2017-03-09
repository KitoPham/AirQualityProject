
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

us.frame <- filter(us.frame, latitude > 25) %>% 
  filter(latitude < 55)








us.frame <-read.csv("data/Mar1-4.csv", stringsAsFactors = FALSE)

map.frame <- map_data("world") %>% 
  mutate(country = iso.alpha(region)) %>% 
    filter(country == "US")



ggplot(data = map.frame, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group)) +
    coord_quickmap()


   


x <- map.cities(x = , country = "", label = TRUE,
          cex = par("cex"), projection = FALSE,
           parameters = NULL, orientation = NULL, pch = 1)





states <- map_data("state")
 

us.frame$value = cut(us.frame$value, 10)
p <- ggplot() + 
  geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
  geom_point(data = us.frame, aes(x = longitude, y = latitude, color = value)) +
      scale_color_brewer(palette = 'Reds')









