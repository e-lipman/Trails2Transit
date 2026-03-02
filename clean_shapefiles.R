library(tidyverse)
library(janitor)
library(sf)

library(mapview)
library(leaflet)

# read spatial data sources
trails <- st_read("https://gismaps.kingcounty.gov/arcgis/rest/services/Parks/KingCo_ParksAndTrails/MapServer/6/query?where=1=1&outFields=*&f=geojson")
bus_stops <- st_read("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/transportation__transitstop_point/MapServer/2648/query?where=1=1&outFields=*&f=geojson")
kc_boundary <- st_read("https://gisdata.kingcounty.gov/arcgis/rest/services/OpenDataPortal/politicl___base/MapServer/121/query?where=1=1&outFields=*&f=geojson")

# clean trails
trails_clean <- trails %>% 
  clean_names() %>% 
  select(trail_name, trail_type, surf_type, site_name, site_type,
         shape_st_length) %>%
  filter(grepl('Trail', site_type),
         !grepl('Site', trail_name)) %>%
  mutate(len_miles_segment=shape_st_length/5280,
         trail_name_clean = gsub(' - .*' ,'',trail_name)) %>%
  # special cleaning for trail names
  mutate(trail_name = gsub("Interuban","Interurban",trail_name),
         trail_name_clean = case_when(!grepl('Interurban', trail_name)~trail_name_clean,
                                      grepl('Connector', trail_name)~trail_name_clean,
                                      grepl('Seattle|Shoreline|Edmonds', trail_name)~
                                        'Interurban Trail - North',
                                      T~'Interurban Trail - South')) %>%
  # length entire trail
  group_by(trail_name_clean) %>%
  mutate(len_miles_trail=sum(len_miles_segment)) %>%
  ungroup() %>%
  # length trail part
  group_by(trail_name) %>%
  mutate(len_miles_part=sum(len_miles_segment)) %>%
  ungroup() %>%
  # # filter out short trails
  filter(len_miles_trail>=1)

mapview(trails_clean)

# clean bus stops
bus_stops_clean <- bus_stops %>% 
  clean_names() %>%
  filter(!is.na(route_list)) %>%
  select(stop_status, cf_cross_streetname, zipcode, route_list, 
         on_street_name, in_service_flag)


# output
out <- list(trails=trails_clean, 
            bus_stops=bus_stops_clean,
            county=kc_boundary)

saveRDS(out, 'clean_geo_data.RDS')
