library(shiny)
my.ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      selectInput("parameter", "Gas Measurement", c("pm25", "pm10","so2", "o3","co","bc"))
    ),
    mainPanel(
      plotOutput("plot")
    )
  ),
  sliderInput("date","Date:",
              min = as.Date("2017-02-07","%Y-%m-%d"),
              max = as.Date("2017-03-07","%Y-%m-%d"),
              value = c(as.Date("2016-03-01","%Y-%m-%d"),
                        as.Date("2017-03-07","%Y-%m-%d")),

              timeFormat = "%Y-%m-%d")
)
