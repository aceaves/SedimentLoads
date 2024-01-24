###############################################################################

# Map for Shiny App

library(shiny)
library(leaflet)
library(dplyr)

# Map dataset
markers_data <- read.csv("I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/app/markers.csv")

ui <- fluidPage(
  selectInput("popup_selector", "Select Popup to Zoom", choices = unique(markers_data$popup)),
  leafletOutput("map")
)

server <- function(input, output, session) {
  map_initialized <- reactiveVal(FALSE)
  
  observe({
    req(input$file)
    
    # Read the CSV file
    markers_data(read.csv(input$file$datapath))
  })
  
  output$map <- renderLeaflet({
    selected_popup <- input$popup_selector
    
    if (!map_initialized()) {
      # Determine the bounds that encompass all markers
      all_markers_bounds <- markers_data %>%
        summarise(lng1 = min(lon), lat1 = min(lat), lng2 = max(lon), lat2 = max(lat))
      
      map_initialized(TRUE)
      
      # Create the map
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = markers_data, lat = ~lat, lng = ~lon, popup = ~popup) %>%
        fitBounds(all_markers_bounds$lng1, all_markers_bounds$lat1, all_markers_bounds$lng2, all_markers_bounds$lat2)
    } else {
      # Create the map without fitting bounds
      leaflet() %>%
        addTiles() %>%
        addMarkers(data = markers_data, lat = ~lat, lng = ~lon, popup = ~popup)
    }
  })
  
  observe({
    selected_popup <- input$popup_selector
    if (!is.null(selected_popup)) {
      leafletProxy("map") %>%
        flyTo(lng = markers_data$lon[markers_data$popup == selected_popup], 
              lat = markers_data$lat[markers_data$popup == selected_popup], 
              zoom = 15)
    }
  })
}

shinyApp(ui, server)