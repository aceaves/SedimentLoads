<<<<<<< HEAD
################################################################################
# This is part of the ISCO Shiny web application. 
#
#App to explore the outputs of the ISCO programme.
#Created 10/05/2023 by Ashton Eaves


####Server side for Shiny IO:

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
  
=======
################################################################################
# This is a Shiny web application. 
#
#App to explore the outputs of the ISCO programme.
#Created 10/05/2023 by Ashton Eaves


####Server side for Shiny IO:

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
  
>>>>>>> origin/main
}