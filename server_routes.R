# set start and end location

start_location <- reactiveVal(NULL)
end_location <- reactiveVal(NULL)

### FIX MAJOR COPY-PASTA IN THIS SECTION!

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


observeEvent(input$search_start, {
  set_start_loc_from_address()
  
  # refactor
  leafletProxy("map") %>%
    clearGroup("start_marker") %>%
    addMarkers(
      lng = start_location()["lng"],
      lat = start_location()["lat"],
      label = "Start",
      group = "start_marker"
    )
})

set_end_loc_from_address <- function(){
  req(input$end_address)
  
  result <- google_geocode(
    address = input$end_address,
    key = Sys.getenv("GOOGLE_API_KEY")
  )
  
  if (result$status != "OK") {
    showNotification(
      paste("Could not find end address:", input$start_address),
      type = "error"
    )
    return()
  }
  
  coords <- geocode_coordinates(result)
  end_location(c(lat = coords$lat[1], lng = coords$lng[1]))
}

observeEvent(input$search_end, {
  set_end_loc_from_address()
  
  # refactor
  leafletProxy("map") %>%
    clearGroup("end_marker") %>%
    addMarkers(
      lng = end_location()["lng"],
      lat = end_location()["lat"],
      label = "End",
      group = "end_marker"
    )
})

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

observeEvent(input$map_click, {
  req(input$end_location_method == "click") # only if we are in click mode
  
  click <- input$map_click
  end_location(c(lat = as.numeric(click$lat), lng = as.numeric(click$lng)))
  
  leafletProxy("map") %>%
    clearGroup("end_marker") %>%
    addMarkers(
      lng = end_location()["lng"],
      lat = end_location()["lat"],
      label = "End",
      group = "end_marker"
    )
})


# get route
get_route <- eventReactive(input$route_action, {
  google_directions(
    origin = c(start_location()["lat"], start_location()["lng"]),
    destination = c(end_location()["lat"], end_location()["lng"]),
    mode = "transit",
    key = Sys.getenv("GOOGLE_API_KEY")
  )
})

# plot
observeEvent(input$route_action, {
  if (is.null(start_location())){
    showNotification(
      paste("Please select a start location"),
      type = "error"
    )  
    return()
  }
  if (is.null(end_location())){
    showNotification(
      paste("Please select as end location"),
      type = "error"
    )  
    return()
  }
  
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

