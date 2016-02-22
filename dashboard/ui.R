library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Dashboard!"),
  
  # Sidebar with a slider input for the number of bins
  
  
  
  fluidRow(
    column(9,
           htmlOutput("crashPages"),
           tableOutput("avgByteTable")
    ),
    column(3,
           helpText("The frequency of crashes is around 27-28 crashes per hour.")
           
    )
  ),
  hr(),
  sidebarLayout(
    sidebarPanel(
      helpText("Move the slider to check crashes at every hour since,"),
      helpText("Thu, 01 Oct 2015 21:12:44 GMT"),
      sliderInput("hours",
                  "Choose an hour",
                  min = 1,
                  max = 28,
                  value = 1),
      textOutput("crashPerHourText"),
      textOutput("avgBytesPerHourText")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("crashPagesHour")
    )
  )
))