# reset buttons
observeEvent(input$reset_filters, {
  # route
  updateRadioButtons(session, "start_location_method", selected = "address")
  updateTextInput(session, "start_address", value = "")
  start_location(NULL)
  
  updateRadioButtons(session, "end_location_method", selected = "address")
  updateTextInput(session, "end_address", value = "")
  end_location(NULL)
  
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
  req(input$steps=='step1_trail')
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
  req(input$steps=='step2_bus')
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


# reactivity for routing mode

## Relegate trails to background when in routing mode
observeEvent(input$steps, {
  leafletProxy("map") %>%
    clearGroup("trails") %>%
    removeControl("trails_legend") %>%
    draw_trails_layer(bkg_mode="step3_route" %in% input$steps)  
  
})

## Clost address input panels when route button pressed
observeEvent(input$route_action, {
  updateCollapse(session, "address_input", close = "start")
  updateCollapse(session, "address_input", close = "end")
})

