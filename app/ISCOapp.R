################################################################################
# This is the main Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Need to format date-time in CSV first!!!!!
#
#App to explore the outputs of the ISCO programme.
#Created 10/05/2023 by Ashton Eaves
#Updated 19/01/2024 by Ashton Eaves

library(shiny)
library(ggplot2)
library(scales)
library(htmltools)
library(hms) 
library(lubridate) 
library(readr)
library(dygraphs)
library(plotly)
library(leaflet)
library(dplyr)
library(sf)
library(shinyWidgets)
library(xml2)

# Load map dataset
#markers_data <- read.csv("I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/app/markers.csv")
markers_data <- read.csv("markers.csv")
# Read shapefile of catchment areas
catchment_polygons <- st_read("ISCO_HBRC_REC2_Catchment_Area_2023_Selection.shp")

# Load sediment concentration data
df <- read_csv("https://media.githubusercontent.com/media/aceaves/SedimentRatingCurves/main/app/measure.csv")
df$SampleTaken <- as.POSIXct(df$SampleTaken, format = "%d/%m/%Y %H:%M")
sitelist <- unique(df$SiteName)

# ggplot code
ggplot_code <- function(df2, input_df) {
  line_color <- ifelse(input_df == "Flow", "blue3",
                       ifelse(input_df == "PredConc", "darkgoldenrod",
                              ifelse(input_df == "Load", "saddlebrown",
                                     ifelse(input_df == "AccumLoad", "darkred", NA))))
  ggplot(data = df2, aes(x = SampleTaken, y = .data[[input_df]])) +
    #geom_line(colour = 'darkgoldenrod') +
    geom_line(colour = line_color) +
    theme_bw() +
    scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::comma_format()) +
    theme(
      axis.text = element_text(colour = 'black', size = 9),
      axis.title = element_text(colour = 'black', size = 12),
      axis.text.x = element_text(angle = 45, hjust = 1)  # Adjust the angle as needed
    ) +
    xlab('Date') + ylab(paste(input_df))
}


# Define UI for the app
ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      /* Custom CSS styles */
      .footer {
        text-align: center;
        margin-top: 20px;
        color: #777;
      }

      .logo img {
        max-width: 100px; /* Adjust the maximum width as needed */
      }
    "))
  ),
  fluidRow(
 #   column(12, align = "center", "Shiny App developed by Land Science for the ISCO Programme"),
      ),
 # Footer with logo and copyright
 tags$div(
   style = "text-align: center; color: #777; position: fixed; bottom: 0; left: 0; width: 100%;",
   tags$div(
     class = "logo",
     img(src = "https://raw.githubusercontent.com/aceaves/SedimentRatingCurves/main/app/HBRC-RGB.jpg", alt = "Company Logo", style = "max-width: 175px;")
   ),
   HTML("<p>© 2024 Hawke's Bay Regional Council. All rights reserved.</p>")
 ),
  
 titlePanel(div("HBRC - Sediment Concentration Plots, Flow & Load Summary", style = "color: #8B6508")),
  
 sidebarLayout(
   sidebarPanel(
     selectInput("SiteName", "Site Name:", c(sitelist)),
     selectInput("df", "Measurement:",
                 c("Flow (m³/s)" = "Flow",
                   "Predicted Concentration SSC (mg/l)" = "PredConc",
                   "Sediment Load (T/s)" = "Load",
                   "Accumulated Load (T)" = "AccumLoad")),
     dateRangeInput("dater","Date range:",start=df$SampleTaken[1],end=df$SampleTaken[nrow(df)]),
   ),
    mainPanel(
      verbatimTextOutput("caption"),
      plotlyOutput("Plot"),
      leafletOutput("map")
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  output$caption <- renderText({
    paste("Measurement ~", input$df, " | Site ~", input$SiteName)
  })
  
  # Shiny app renderPlot
  output$Plot <- renderPlotly({
    df1 <- subset(df, SiteName == input$SiteName)
    df2 <- df1[df1$SampleTaken >= as.POSIXct(input$dater[1]) & df1$SampleTaken <= as.POSIXct(input$dater[2]),]
    ggplot_code(df2, input$df) %>%
      ggplotly()
  })
  
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
        addProviderTiles("Esri.WorldImagery") %>%  # Satellite imagery
        addProviderTiles("Esri.WorldStreetMap", group = "Labels") %>%  # Street labels
        addMarkers(data = markers_data, lat = ~lat, lng = ~lon, popup = ~popup) %>%
        addPolygons(data = catchment_polygons, fillColor = "orange", fillOpacity = 0.5, popup = ~SiteName) %>%
        fitBounds(all_markers_bounds$lng1, all_markers_bounds$lat1, all_markers_bounds$lng2, all_markers_bounds$lat2) %>%
        addLayersControl(baseGroups = c("Esri.WorldImagery", "Labels"))
    } else {
      # Create the map without fitting bounds
      leaflet() %>%
        addTiles() %>%
        addProviderTiles("Esri.WorldImagery") %>%  # Satellite imagery
        addProviderTiles("Esri.WorldStreetMap", group = "Labels") %>%  # Street labels
        addMarkers(data = markers_data, lat = ~lat, lng = ~lon, popup = ~popup) %>%
        addPolygons(data = catchment_polygons, fillColor = "orange", fillOpacity = 0.5, popup = ~SiteName) %>%
        addLayersControl(baseGroups = c("Esri.WorldImagery", "Labels"))
    }
  })
  observe({
    selected_popup <- input$popup_selector
    if (!is.null(selected_popup)) {
      leafletProxy("map") %>%
        flyTo(lng = markers_data$lon[markers_data$popup == selected_popup], 
              lat = markers_data$lat[markers_data$popup == selected_popup], 
              zoom = 14)
    }
  })
  observe({
    selected_site <- input$SiteName
    if (!is.null(selected_site)) {
      leafletProxy("map") %>%
        flyTo(lng = markers_data$lon[markers_data$popup == input$SiteName], 
              lat = markers_data$lat[markers_data$popup == input$SiteName], 
              zoom = 14)
    }
  })
  observeEvent(input$SiteName, {
    leafletProxy("map") %>%
      flyTo(lng = markers_data$lon[markers_data$popup == input$SiteName], 
            lat = markers_data$lat[markers_data$popup == input$SiteName], 
            zoom = 14)
  })
}

##### Turn on ONE of the options below:
#For localhost:
shinyApp(ui, server)

#For port forwarding:
#shinyApp(ui, server, options = list(port = 3093, host = "192.168.5.108"))


#runApp("app",host="0.0.0.0",port=3000)
#my IP = "192.168.5.96"

###############################################################################