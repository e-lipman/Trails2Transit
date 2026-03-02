#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(mapview)
library(leaflet)
library(DT)

# Define UI for application that draws a histogram
fluidPage(

    # Application title
    titlePanel("TRANSIT 2 TRAILS"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            h3("STEP 1: Explore Trails"),
            textInput(
              'trail_name',
              'Search for a specific trail:',
              value = ""),
            sliderInput('min_length',
                        "Minimum trail length (mi):",
                        min = 0,
                        max = 20,
                        value = 1),
            
            h3("STEP 2: Explore bus routes:"),
            checkboxInput(
              inputId = "show_bus_stops",
              label   = "Search for bus stops near selected trails?",
              value   = FALSE  # default checked state
            ),
            conditionalPanel(
              condition = "input.show_bus_stops == true",  # note: lowercase true (JavaScript)
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
            ),
        ),

        # Show a plot of the generated distribution
        mainPanel(
          leafletOutput("map", height = "500px"),
          #h2("Trails by length:"),
          #tableOutput("table")
        )
    )
)
