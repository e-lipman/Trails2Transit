library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(tidyverse)
library(DT)

library(googleway)
library(glue)


# static objects
dat_geo <- readRDS('clean_geo_data.RDS')
dat_geo$trails <- dat_geo$trails %>% mutate(trail_id=1:n())
dat_geo$bus_stops <- dat_geo$bus_stops %>% mutate(stop_id=1:n())




