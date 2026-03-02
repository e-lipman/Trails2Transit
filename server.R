#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(mapview)
library(leaflet)
library(tidyverse)
library(DT)

# static objects
dat_geo <- readRDS('clean_geo_data.RDS')
dat_geo$trails <- dat_geo$trails %>% mutate(trail_id=1:n())
dat_geo$bus_stops <- dat_geo$bus_stops %>% mutate(stop_id=1:n())


# Define server logic required to draw a histogram
function(input, output, session) {
  # reactive updates to inputs
  
  ## Selected trail from clicking
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
  
  ## Bus routes for clicked bus stop
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
  
  ## reset buttons
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
  
  
    # reactive helper funcs
  
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
    
    # outputs
    
    output$table <- renderTable({
      trails_menu <- trails_filtered_func() %>%
        distinct(trail_name_clean, len_miles_trail) %>%
        arrange(desc(len_miles_trail))
    })
    
    output$map_old <- renderLeaflet({

      # trails
      trails_filtered <- trails_filtered_func()
      
      trails_label <- 
        paste0(# trail name   
          "<b>", trails_filtered$trail_name_clean, '</b>', 
          # trail length 
          "<br><b>Trail length: </b>",  
          round(trails_filtered$len_miles_trail,1),  
          ' miles', 
          # trail part name 
          "<br><b>Trail part: </b>",  
          trails_filtered$trail_name, '</b>', 
          # trail part len
          "<br><b>Trail part length: </b>",  
          round(trails_filtered$len_miles_part, 1), '</b>', 
          # segment surface type 
          "<br><b>Segment surface type: </b>",  
          trails_filtered$surf_type, '</b>',
          # segment length 
          "<br><b>Segment length: </b>",  
          round(trails_filtered$len_miles_segment, 1), '</b>' 
        )
      
      surf_color_map <- c("Paved Trail" = "gray",  
                          "On Street Trail" = "blue",  
                          "Soft Surface Trail" = "brown")
      trails_filitered <- trails_filtered %>%
        mutate(surf_type=fct_relevel(surf_type, names(surf_color_map)))
        
      
      m_trails <- mapview(trails_filtered,  
                          label=trails_label,  
                          zcol='surf_type',
                          color = surf_color_map[levels(trails_filitered$surf_type)],
                          lwd=5, 
                          popup=F) 
      m <- m_trails
      
      # bus stops
      if (input$show_bus_stops){
        bus_stops_filtered <- bus_stops_filtered_func()
        
        stops_label <- 
          paste0( 
            # Route numbers 
            "<br><b>Routes: </b>",  
            bus_stops_filtered$route_list, '</b>' 
          )
        
        m_stops <- mapview(bus_stops_filtered,   
                           label=stops_label, 
                           cex=5, 
                           legend=F)
        
        m <- m + m_stops
      } 
      
      # county boundary
      m_county <- mapview(dat_geo$county,
                          color='blue',
                          col.regions = NA,
                          alpha.regions = 0,
                          label='',
                          popup = F,
                          legend=F)
      m <- m + m_county
      
      m@map
    })
    
    output$map <- renderLeaflet({
      
      # trails
      trails_filtered <- trails_filtered_func()
      
      surf_color_map <- c("Paved Trail" = "gray",
                          "On Street Trail" = "blue",
                          "Soft Surface Trail" = "brown")
      
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
      
      m <- leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolylines(
          data = trails_filtered,
          #color = ~surf_type,
          weight = 5,
          layerId = ~trail_id,
          label = lapply(trails_label, HTML),  # render HTML in labels
          highlightOptions = highlightOptions(
            color = "yellow",
            weight = 10,
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "topright",
          colors = unname(surf_color_map),
          labels = names(surf_color_map),
          title = "Surface Type"
        )
      
      # bus stops
      if (input$show_bus_stops) {
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
      
      # county boundary
      m <- m %>%
        addPolylines(
          data = dat_geo$county,
          color = "blue",
          weight = 2,
          fill = FALSE
        )
      
      m
    })

}
