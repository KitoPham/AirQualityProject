
library(httr)
library(jsonlite)
library(maps)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)


us.frame <-read.csv("data/Mar1-4.csv", stringsAsFactors = FALSE)

map.frame <- map_data("world") %>% 
  mutate(country = iso.alpha(region)) %>% 
    filter(country == "US")


ggplot(data = map.frame, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group)) +
    coord_quickmap()

   