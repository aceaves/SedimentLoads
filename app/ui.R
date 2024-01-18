################################################################################
# This is part of the ISCO Shiny web application.
#
#App to explore the outputs of the ISCO programme.
#Created 10/05/2023 by Ashton Eaves


### UI for IO deployment:

ui <- fluidPage(
  
  # App title ----
  titlePanel("Sediment Plots, Flow & Load Summary"),
  
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

################################################################################