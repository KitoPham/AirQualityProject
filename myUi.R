library(shiny)
my.ui <- fluidpage(
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter", "Select a gas", c("pm25", "pm10","so2", "o3","co","bc"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
  sliderInput("date", timeFormat = "%F")
)