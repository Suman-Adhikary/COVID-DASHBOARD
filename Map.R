library(tidyverse)
library(sf)
library(plotly)
library(ggplot2)

## Covid data.
dataset <- read.csv(file = "C://Users//suman//Desktop//Project//Covid Dashboard//Data//Map_data.csv")
dataset <- dataset[!(dataset$Code == "TT"),]
dataset <- dataset %>%
  select(States, Code, Confirmed, Recovered, Deceased) %>%
  group_by(States) %>%
  summarise(Code = unique(Code), Confirmed = sum(Confirmed), Recovered = sum(Recovered), Deceased = sum(Deceased))
dataset <- dataset[!(dataset$States == "Daman and Diu"),]

## Map data.
map = read_sf("https://gist.githubusercontent.com/jbrobst/56c13bbbf9d97d187fea01ca62ea5112/raw/e388c4cae20aa53cb5090210a42ebb9b765c0a36/india_states.geojson")
centroids <- map %>%
  st_centroid() %>%
  bind_cols(as_data_frame(st_coordinates(.)))

## Joinint the data
lat_long <- dataset %>%
  left_join(centroids, ., by = c('ST_NM' = 'States'))


## Modified Column name.
colnames(lat_long) <- c("ST_NM", "geometry", "latitude", "longitude", "Code", "Confirmed", "Recovered", "Deceased")

## Map plot.
plt <- dataset %>%
  left_join(map, ., by = c('ST_NM' = 'States')) %>%
  ggplot() +
  geom_sf(fill = alpha("#B9E1C9", 1), color = "white", alpha = 0.5) +
  geom_point(data = lat_long , aes(size = Confirmed, label = Deceased, label2 = Recovered, x = latitude, y = longitude), color = factor(dataset$Confirmed), alpha = 0.5) +
  geom_text(data = lat_long, aes(label = ST_NM, x = latitude, y = longitude), size = 3) +
  scale_size(
    range = c(1, 30)
  ) + 
  theme_void() + 
  theme(axis.line.x = element_blank(),
        axis.line.y = element_blank()) +
  theme(legend.position = "none")

ggplotly(plt) %>%
  config(displayModeBar = F)

