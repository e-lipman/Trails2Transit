# reset buttons
observeEvent(input$reset_filters, {
  # bus
  updateCheckboxInput(session, "show_bus_stops", value=FALSE)
  updateSelectInput(session, "bus_route_filter", selected = "")
  updateSelectInput(session, "bus_stop_buffer", selected = ".5")
  # trail
  updateSelectInput(session, "trail_name", selected = "")
  updateSelectInput(session, "min_length", selected = 1)
})

observeEvent(input$reset_route, {
  updateSelectInput(session, "bus_route_filter", selected = "")
  updateSelectInput(session, "bus_stop_buffer", selected = ".5")
})


# select trail from map
clicked_trail_name <- reactiveVal(NULL)

observeEvent(input$map_shape_click, {
  click_trail_id <- input$map_shape_click$id
  
  # look up the route_list for the clicked stop
  clicked_trail <- trails_filtered_func() %>%
    filter(trail_id == click_trail_id)
  
  # update input params
  clicked_trail_name(clicked_trail$trail_name_clean)
  
  updateSelectInput(session, "trail_name",
                    selected = clicked_trail_name())
  
})

# select bus routes from map
clicked_routes <- reactiveVal(NULL)

observeEvent(input$map_marker_click, {
  click_id <- input$map_marker_click$id
  
  # look up the route_list for the clicked stop
  clicked_stop <- bus_stops_filtered_func() %>%
    filter(stop_id == click_id)
  
  # update input params
  clicked_routes(clicked_stop$route_list)
  
  updateSelectInput(session, "bus_route_filter",
                    selected = clicked_routes())
  updateNumericInput(session, 'bus_stop_buffer',
                     value = 1000)
})


#reactivity for routing
start_location <- reactiveVal(NULL)

get_route <- reactive({
  google_directions(
    origin = c(start_location()["lat"], start_location()["lng"]) %>% as.numeric(),
    destination = c(end_location["lat"], end_location["lng"]) %>% as.numeric(),
    mode = "transit",
    key = Sys.getenv("GOOGLE_API_KEY")
  )
})

observeEvent(input$route_action, {
  req(input$start_address)
  
  result <- google_geocode(
    address = input$start_address,
    key = Sys.getenv("GOOGLE_API_KEY")
  )
  
  if (result$status != "OK") {
    showNotification(
      paste("Could not find address:", input$start_address),
      type = "error"
    )
    return()
  }
  
  coords <- geocode_coordinates(result)
  start_location(c(lat = coords$lat[1], lng = coords$lng[1]))
  
  leafletProxy("map") %>%
    clearGroup("start_marker") %>%
    addMarkers(
      lng = start_location()["lng"],
      lat = start_location()["lat"],
      label = result$results$formatted_address,
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

