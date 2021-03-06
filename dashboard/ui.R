library(shiny)
shinyUI(fluidPage(
  # Application title
  titlePanel("Dashboard!"),
  sidebarLayout(
    position = "left",
    mainPanel(
           htmlOutput("crashPages"),
           helpText("From the bar graph on the top, we can see that the root page and analytics page has crashed the most."),
           helpText("From the table to the right we can see that average memory consumption of each page is almost the same.")
           
    ),
    sidebarPanel(
           helpText("Average memory consumption by each page."),
           tableOutput("avgByteTable")
    )
  ),
  hr(),
  helpText("Also, 66% of the crashes occur if the bytes used exceed around 176.4 Mb."),
  helpText("The frequency of crashes is around 27-28 crashes per hour."),
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
      textOutput("crashPerHourText")
      #textOutput("avgBytesPerHourText")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      htmlOutput("crashPagesHour")
    )
  )
))