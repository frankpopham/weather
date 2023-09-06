library(tidyverse)
library(leaflet)
library(htmltools)

# read in dataset

df <- read_rds("month_weather_station.RDS")

# just need  details of stations

df_station <- df %>%
  group_by(name) %>%
  slice_head() %>%
  select(name:latitude)

# basic map

mp <- leaflet(df_station) %>%
  addTiles() %>%
  setView(lng = -4, lat = 55.5, zoom = 5) %>%
  addMarkers(label=~htmlEscape(name))