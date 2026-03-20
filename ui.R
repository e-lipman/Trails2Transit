#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


# Define UI for application that draws a histogram
fluidPage(
  
  # Application title
  titlePanel("TRANSIT 2 TRAILS"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      actionButton("reset_filters", "Reset search"),
      
      bsCollapse(id = "steps", open = "step1_trail", multiple=FALSE,
                 
                 bsCollapsePanel("STEP 1: Explore Trails", value = "step1_trail",
                                 textInput(
                                   'trail_name',
                                   'Search for a specific trail:',
                                   value = ""),
                                 helpText('Enter a trail name or click on a trail on the map to select.'),
                                 sliderInput('min_length',
                                             "Minimum trail length (mi):",
                                             min = 0,
                                             max = 20,
                                             value = 1)
                 ),
                 
                 bsCollapsePanel("STEP 2: Explore Bus Routes", value = "step2_bus", 
                                 checkboxInput(
                                   inputId = "show_bus_stops",
                                   label   = "Search for bus stops near selected trails?",
                                   value   = FALSE  # default checked state
                                 ),
                                 helpText(paste0('It is best to filter to specific routes before ',
                                                 'selecting this option to avoid long load times.')),
                                 #conditionalPanel(
                                  # condition = "input.show_bus_stops == true",  # note: lowercase true (JavaScript)
                                   numericInput('bus_stop_buffer',
                                                "Bus stop buffer (mi):",
                                                min = 0,
                                                max = 1000,
                                                step=.1,
                                                value = .5),
                                   textInput(
                                     'bus_route_filter',
                                     'Search for a specific bus route:',
                                     value = ""),
                                   helpText('Enter a route number or click on a bus stop on the map to see ',
                                            "all routes going to that stop.")
                                 #)
                 ),
                 
                 bsCollapsePanel("STEP 3: Routing", value = "step3_route",
                                 radioButtons("start_location_method", 
                                              label = "Set start location by:",
                                              choices = c("Address" = "address", "Map click" = "click"),
                                              selected = "address"),
                                 ## by address
                                 conditionalPanel(
                                   condition = "input.start_location_method == 'address'",
                                   textInput("start_address", "Enter address for start location:", 
                                             placeholder = "Type an address..."),
                                   actionButton("search_start", "Search", style = "margin-top: 25px"),
                                 ),
                                 ## By click
                                 conditionalPanel(
                                   condition = "input.start_location_method == 'click'",
                                   helpText("Click anywhere on the map to set your start location")
                                 ),
                                 
                                 # end location
                                 h4('Select end location'),
                                 radioButtons("end_location_method", 
                                              label = "Set end location by:",
                                              choices = c("Address" = "address", "Map click" = "click"),
                                              selected = "address"),
                                 ## by address
                                 conditionalPanel(
                                   condition = "input.end_location_method == 'address'",
                                   textInput("end_address", "Enter address for end location:", 
                                             placeholder = "Type an address..."),
                                   actionButton("search_end", "Search", style = "margin-top: 25px"),
                                 ),
                                 ## By click
                                 conditionalPanel(
                                   condition = "input.end_location_method == 'click'",
                                   helpText("Click anywhere on the map to set your end location")
                                 ),
                                 
                                 actionButton("route_action", "Route!", style = "margin-top: 25px")
                 )
      )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      leafletOutput("map", height = "500px"),
      conditionalPanel(
        condition = "input.steps == 'step3_route'", 
        h3('Route info'), 
        tableOutput("route_table")
      )
    )
  )
)