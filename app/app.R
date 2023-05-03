#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(Hilltop)
library(dplyr)
library(scales)
library(ggplot2)
library(tidyverse)
library(hms) 
library(lubridate) 
library(gt)

################################################################################
# Data load

#set file path to ISCO Hilltop file
dfile <- HilltopData("N:/HilltopData/WQ_E_Working/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites
site <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)

Hilltop::SiteList(dfile)

# Date range.
date1 <- "01-Mar-2021 00:00:00"
date2 <- "01-Mar-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow")

#Output from Sediment_rating_curves_working.R
rating_output <- test

################################################################################

# Define UI
ui <- fluidPage(
  navbarPage("Shiny App", tabPanel("Main Tab"),
             tabPanel("Tab2", "Work In Progress")),
  
  # Application title
  titlePanel("Sediment Rating Curves & Summary"),
  
  # Add slider input for time range
  sliderInput("time_range", "Select time range:",
              min = as.POSIXct("2022-01-01"), 
              max = as.POSIXct("2022-12-31"), 
              value = c(as.POSIXct("2022-01-01"), as.POSIXct("2022-12-31")),
              timeFormat = "%Y-%m-%d",
              timezone = "UTC"
  ),
  
  # Add select input for site
  uiOutput("site_input"),
  
  # Add select input for measurements
  uiOutput("measurement_input"),
  
  selectInput("test", "test:", 
              c("Flow", "Flowlog", "concLog", "predConc", "load", "acc_sum", "summary")  
  ),
  
  # Display the selected time range and data
  verbatimTextOutput("result")
)


#-------------------------------------------------------------------------------
# Define server
server <- function(input, output) {
  
  # Create reactive expression for selected time range
  time_range_selected <- reactive({
    range <- input$time_range
    return(paste(format(range[1], "%Y-%m-%d"), " to ", format(range[2], "%Y-%m-%d")))
  })
  
  # Create reactive expression for selected data
  data_selected <- reactive({
    # Read data from file
    df <- rating_output
    # Filter data based on user input
    df <- subset(df, site_name == input$site)
    df <- subset(df, Measurement == input$measurement)
    
    # Return filtered data
    return(df)
  })
  
  # Render the selected time range and data
  output$result <- renderPrint({
    time_range <- time_range_selected()
    df <- data_selected()
    paste("Selected time range:", time_range, "\nSelected data:", paste(df, collapse = ", "))
  })
  
  # Create reactive expression for available sites
  available_sites <- reactive({
    unique(site$name)
  })
  
  # Render the available sites in the site selectInput
  observe({
    choices <- available_sites()
    updateSelectInput(session, "site", choices = choices)
  })
  
}

# Run the app with the measurement argument passed to the server function

shinyApp(ui, server)

################################################################################

# ################################################################################
# 
# # Define server logic to plot various variables against mpg ----
# server <- function(input, output) {
#   data <- reactive({
#     # Read data from file
#     df <- test
#     
#     # Filter data based on user input
#     df <- subset(df, variable == input$variable)
#     
#     # Return filtered data
#     return(df)
#   })
#   
#   # Generate output
#   output$plot <- renderPlot({
#     # Generate plot based on filtered data
#     Flowplot <- ggplot(data = test) +
#       geom_path(aes(x = SampleTaken, y = Flow), colour = 'black', size = 0.4) + theme_bw() +
#       scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
#       scale_y_continuous(labels = comma_format())+
#       theme(axis.text = element_text(colour = 'black', size = 10), axis.title  = element_text(colour = 'black', size = 10)) +
#       xlab('Date') + ylab('Flow (l/s)')
#   })
# }
# 
# ################################################################################
