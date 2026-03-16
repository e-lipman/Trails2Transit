# enter routing mode
observeEvent(input$routing_mode, {
  print('here')
  
  #leafletProxy("map") %>%
  #  clearGroup("trails")
})


# set start location

start_location <- reactiveVal(NULL)

# set by address helpers
set_start_loc_from_address <- function(){
  req(input$start_address)
  
  result <- google_geocode(
    address = input$start_address,
    key = Sys.getenv("GOOGLE_API_KEY")
  )
  
  if (result$status != "OK") {
    showNotification(
      paste("Could not find start address:", input$start_address),
      type = "error"
    )
    return()
  }
  
  coords <- geocode_coordinates(result)
  start_location(c(lat = coords$lat[1], lng = coords$lng[1]))
}

# set by map click
observeEvent(input$map_click, {
  req(input$start_location_method == "click") # only if we are in click mode
  
  click <- input$map_click
  start_location(c(lat = as.numeric(click$lat), lng = as.numeric(click$lng)))
  
  leafletProxy("map") %>%
    clearGroup("start_marker") %>%
    addMarkers(
      lng = start_location()["lng"],
      lat = start_location()["lat"],
      label = "Start",
      group = "start_marker"
    )
})

# route action!
observeEvent(input$route_action, {
  set_start_loc_from_address()
})



# get route
get_route <- reactive({
  google_directions(
    origin = c(start_location()["lat"], start_location()["lng"]) %>% as.numeric(),
    destination = c(end_location["lat"], end_location["lng"]) %>% as.numeric(),
    mode = "transit",
    key = Sys.getenv("GOOGLE_API_KEY")
  )
})

# plot
observeEvent(input$route_action, {
  
  leafletProxy("map") %>%
    clearGroup("start_marker") %>%
    addMarkers(
      lng = start_location()["lng"],
      lat = start_location()["lat"],
      #label = result$results$formatted_address,
      label='Start',
      group = "start_marker"
    )
  
  # end location
  
  leafletProxy("map") %>%
    clearGroup("end_marker") %>%
    addMarkers(
      lng = as.numeric(end_location["lng"]),
      lat = as.numeric(end_location["lat"]),
      label = "End",
      group = "end_marker"
    )
  
  # route
  route_geo <- googleway::decode_pl(
    get_route()$routes$overview_polyline$points
  )
  
  leafletProxy("map") %>%
    clearGroup("route") %>%
    addPolylines(
      lat = route_geo$lat,
      lng = route_geo$lon,
      color = "black",
      weight = 4,
      group = "route"
    )
})

