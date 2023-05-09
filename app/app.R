#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(scales)

df <- read.csv("M:/E_Science/Projects/306 HCE Project/R_analysis/Rating curves/git/Outputs/measure.csv")
df$SampleTaken<-as.POSIXct(df$SampleTaken, format="%Y-%m-%d %H:%M:%S")

################################################################################

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Sediment Rating Curves, Flow & Load Summary"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      # Input: Selector for variable to plot against ----
      selectInput("SiteName", "Site Name:", 
                  c(sitelist)),
      selectInput("measure", "Measurement:",
                  c("Flow (l/s)" = "Flow",
                    "Predicted Concentration SSC (mg/l)" = "predConc",
                    "Load SSC (mg)" = "load",
                    "Accumulated Load (T)" = "AccumLoadSite",
                    "Summary Load All Sites (T)" = "SummaryAllSites")),
      dateRangeInput("dater","Date range:",start=df$SampleTaken[1],end=df$SampleTaken[nrow(df)]),
      
      # Input: Checkbox for whether outliers should be included ----
      #      checkboxInput("outliers", "Show outliers", TRUE)
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Formatted text for caption ----
      h3(textOutput("caption")),
      
      # Output: Plot of the requested variable against mpg ----
      plotOutput("Plot")
      
    )
  )
)

# Data pre-processing ----
# Tweak the "am" variable to have nicer factor labels -- since this
# doesn't rely on any user inputs, we can do this once at startup
# and then use the value throughout the lifetime of the app


head# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  # Return the formula text for printing as a caption ----
  output$caption <- renderText({
    paste("Measurement ~", input$measure," | Site ~", input$SiteName)
    #   formulaText()
  })
  
  # Generate a plot of the requested variable ----
  output$Plot <- renderPlot({
    # Filter data based on user input
    df1 <- subset(df, SiteName == input$SiteName)
    df2 <- df1[df1$SampleTaken>=input$dater[1] & df1$SampleTaken<=input$dater[2],]
    # Generate plot based on filtered data
    ggplot(data = df2) +
      geom_path(aes(x = SampleTaken, y = df2[[input$measure]]), colour = 'black', size = 0.4) + theme_bw() +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "6 months") +
      scale_y_continuous(labels = comma_format())+
      theme(axis.text = element_text(colour = 'black', size = 12), axis.title  = element_text(colour = 'black', size = 12)) +
      xlab('Date') + ylab(paste(input$measure))
  })
  
}

shinyApp(ui, server)
################################################################################
