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

#set file path to ISCO Hilltop file 
dfile <- HilltopData("N:/HilltopData/WQ_E_Working/ISCO_Processing.dsn")
#dfile <- HilltopData("N:/HilltopData/EMAR/EMARFull.dsn")

# Get measurement list for respective sites 
#measurementlist <- Hilltop::MeasurementList(dfile,"Tukituki River at Red Bridge")
sitelist <- SiteList(dfile, "")

#ISCO sites 
#ISCOSites <-  c( "Tutaekuri River at Puketapu HBRC Site", "Tukituki River at Red Bridge", "Karamu Stream at Floodgates", "Esk River at Waipunga Bridge", "Mangaone River at Rissington", "Maraetotara River at Waimarama Road")
Sites <-     "Tukituki River at Red Bridge"   

#Sites <-  c("Tukituki River at Red Bridge")

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
                  c(sitelist))
    ),
    
    # Show a plot of the generated distribution
    mainPanel()
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
