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

setwd("I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/app")
df <- read.csv("I:/306 HCE Project/R_analysis/Rating curves/RatingCurvesGit/app/measure.csv")
df$SampleTaken<-as.POSIXct(df$SampleTaken, format = "%d/%m/%Y %H:%M")
# Get a unique list of site names
sitelist <- unique(df$SiteName)

################################################################################

# Define UI for the app
ui <- fluidPage(
  titlePanel("Sediment Concentration Plots, Flow & Load Summary"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("SiteName", "Site Name:", c(sitelist)),
      selectInput("df", "Measurement:",
                  c("Flow (l/s)" = "Flow",
                    "Predicted Concentration SSC (mg/l)" = "predConc",
                    "Load SSC (mg)" = "load",
                    "Accumulated Load (T)" = "AccumLoadSite",
                    "Summary Load All Sites (T)" = "SummaryAllSites")),
      dateRangeInput("dater","Date range:",start=df$SampleTaken[1],end=df$SampleTaken[nrow(df)])
    ),
    mainPanel(
      verbatimTextOutput("caption"),
      plotOutput("Plot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$caption <- renderText({
    paste("Measurement ~", input$df, " | Site ~", input$SiteName)
  })
  
  output$Plot <- renderPlot({
    df1 <- subset(df, SiteName == input$SiteName)
    df2 <- df1[df1$SampleTaken >= as.POSIXct(input$dater[1]) & df1$SampleTaken <= as.POSIXct(input$dater[2]),]
    
    ggplot(data = df2, aes(x = SampleTaken, y = df2[[input$df]])) +
      geom_line() +
      theme_bw() +
      scale_x_datetime(date_labels = "%b %Y", date_breaks = "1 month") +
      scale_y_continuous(labels = scales::comma_format()) +
      theme(axis.text = element_text(colour = 'black', size = 12),
            axis.title = element_text(colour = 'black', size = 12)) +
      xlab('Date') + ylab(paste(input$df))
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