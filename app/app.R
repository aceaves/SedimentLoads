#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
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
sitelist <- SiteList(dfile, "")
#measurementlist <- Hilltop::MeasurementList(dfile, sitelist)

Hilltop::SiteList(dfile)

# Date range. 
date1 <- "01-Mar-2021 00:00:00"
date2 <- "01-Mar-2023 00:00:00"

#Measurements/data that we want to pull from the Hilltop file 
measurement <- c(	'Suspended Solids [Suspended Solids]','Suspended Sediment Concentration', "Flow") 

################################################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Shiny App", tabPanel("Main Tab"),
             tabPanel("Tab2", "Work In Progress")),
  
  # Application title
  titlePanel("Sediment Rating Curves & Summary"),
  
  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      selectInput("variable", "Variable:", 
                  c(sitelist)),
      selectInput("variable", "Variable:", 
                  c(sitelist))      
    ),
    
  # Show a plot of the generated distribution
  mainPanel()
  )
)

################################################################################

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  data <- reactive({
    # Read data from file
    df <- read.csv("data.csv")
    
    # Filter data based on user input
    df <- subset(df, variable == input$variable)
    
    # Return filtered data
    return(df)
  })
  
  # Generate output
  output$plot <- renderPlot({
    # Generate plot based on filtered data
    ggplot(data(), aes(x = x, y = y)) + geom_point()
  })
}

################################################################################


# Run the application 
shinyApp(ui, server)
