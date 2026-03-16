
# outputs

output$table <- renderTable({
  trails_menu <- trails_filtered_func() %>%
    distinct(trail_name_clean, len_miles_trail) %>%
    arrange(desc(len_miles_trail))
})

output$map <- renderLeaflet({
  
  m <- leaflet() %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolylines(
      data = dat_geo$county,
      color = "black",
      weight = 1,
      fill = FALSE
    )
  
  # trails
  m <- draw_trails_layer(m)
  
  if (input$show_bus_stops) {
    m <- draw_bus_stops_layer(m)
  }
  
  # route test
  #m <-  m %>%
  #  addPolylines(
  #  lng = route_geo$lon,
  #  lat = route_geo$lat,
  #  color = "black",
  #  weight = 4,
  #  group = "route"
  #)
  
  #leafletProxy("map") %>%
  #clearGroup("route") %>%
  #addPolylines(
  #lng = route_geo$lon,
  #lat = route_geo$lat,
  #color = "pink",
  #weight = 4,
  #group = "route"
  #)
  
  m
})

# Table with transit info
output$route_table <- renderTable({
  
  route <- get_route()
  
  steps <- route$routes$legs[[1]]$steps[[1]]
  
  steps_info <- tibble(mode=steps$travel_mode,
                       route=steps$transit_details$line$short_name, 
                       time_elapsed=steps$duration$text,
                       distance=steps$distance$text,
                       depart_time=steps$transit_details$departure_time$text,
                       arrive_time=steps$transit_details$arrival_time$text,
                       depart_stop=steps$transit_details$departure_stop$name,
                       arrive_stop=steps$transit_details$arrival_stop$name)
  
  
  out <- steps_info %>%
    mutate(step=ifelse(mode=='TRANSIT', 
                       as.character(glue('{route} ({depart_stop} -> {arrive_stop})')), 
                       str_to_title(mode)),
           time_distance_details=ifelse(mode=='TRANSIT',
                                        as.character(glue('{time_elapsed} ({depart_time} - {arrive_time})')),
                                        as.character(glue('{time_elapsed} ({distance})'))
           )) %>%
    select(step,time_distance_details)
  
  return(out)
})
