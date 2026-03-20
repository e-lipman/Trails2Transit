# reactive functions for plotting and updating leaflet maps

## helpers (filtering)

trails_filtered_func <- reactive({
  TRAIL_SEARCH <- input$trail_name
  TRAIL_MIN_LEN = input$min_length
  
  # filter trails
  dat_geo$trails %>%
    filter(surf_type!='Under Construction') %>%
    filter(grepl(TRAIL_SEARCH, trail_name_clean, ignore.case=T),
           len_miles_trail>=TRAIL_MIN_LEN) %>%
    mutate(dummy_id=as.character(row_number()))
})

bus_stops_filtered_func <- reactive({
  trails_filtered <- trails_filtered_func()
  
  BUS_STOP_BUFFER=input$bus_stop_buffer
  BUS_ROUTE_FILTER=input$bus_route_filter
  
  # Buffer distance in meters (1 mile = 1609.34 me  ters)
  buffer_distance_meters <- 1609.34*BUS_STOP_BUFFER 
  buffered_sf <- st_buffer(trails_filtered, dist = buffer_distance_meters)
  
  # get bus stops in buffer
  bus_stops_filtered <- 
    st_filter(dat_geo$bus_stops, buffered_sf) %>% 
    filter(grepl(BUS_ROUTE_FILTER, route_list), 
           in_service_flag=='Y')
  
})


## map drawing helpers
draw_trails_layer <-  function(m, bkg_mode=F){
    trails_filtered <- trails_filtered_func()
    
    surf_color_map <- c("Paved Trail" = "blue",
                        "On Street Trail" = "gray",
                        "Soft Surface Trail" = "red")
    
    trails_filtered <- trails_filtered %>%
      mutate(
        surf_type = fct_relevel(surf_type, names(surf_color_map)),
        color = surf_color_map[as.character(surf_type)]
      )
    
    trails_label <- paste0(
      "<b>", trails_filtered$trail_name_clean, "</b>",
      "<br><b>Trail length: </b>", round(trails_filtered$len_miles_trail, 1), " miles",
      "<br><b>Trail part: </b>", trails_filtered$trail_name,
      "<br><b>Trail part length: </b>", round(trails_filtered$len_miles_part, 1),
      "<br><b>Segment surface type: </b>", trails_filtered$surf_type,
      "<br><b>Segment length: </b>", round(trails_filtered$len_miles_segment, 1)
    )
    
    if (!bkg_mode){
      out <- m %>% 
        addPolylines(
          data = trails_filtered,
          color = unname(trails_filtered$color),
          weight = 5,
          layerId = ~trail_id,
          label = lapply(trails_label, HTML),  # render HTML in labels
          highlightOptions = highlightOptions(
            color = "yellow",
            weight = 10,
            bringToFront = TRUE
          ),
          group='trails'
        ) %>%
        addLegend(
          position = "topright",
          colors = unname(surf_color_map),
          labels = names(surf_color_map),
          title = "Surface Type",
          layerId = 'trails_legend'
        )  
    } else {
      out <- m %>% 
        addPolylines(
          data = trails_filtered,
          color = 'gray',
          weight = 2,
          layerId = ~trail_id,
          label = lapply(trails_label, HTML),  # render HTML in labels
          group='trails'
        ) 
    }
    
    
    out 
}

draw_bus_stops_layer <- function(m){
  
    bus_stops_filtered <- bus_stops_filtered_func()
    
    stops_label <- paste0(
      "<b>Routes: </b>", bus_stops_filtered$route_list
    )
    
    m <- m %>%
      addCircleMarkers(
        data = bus_stops_filtered,
        radius = 5,
        label = lapply(stops_label, HTML),
        stroke = FALSE,
        fillOpacity = 0.8,
        layerId = ~stop_id
      )
}




