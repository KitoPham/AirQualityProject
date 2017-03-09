
library(httr)
library(jsonlite)
library(maps)
library(knitr)
library(ggplot2)
library(dplyr)
library(tidyr)


us.frame <-read.csv("data/1-4.csv", stringsAsFactors = FALSE)

map.frame <- map_data("br") %>% 
  mutate(country = iso.alpha(region))
airData <- left_join(map.frame,airData)

ggplot(data = airData, na.rm = TRUE) +
    geom_polygon(aes(x = long, y = lat, group = group, fill = value)) +
    coord_quickmap()

   