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
              min = as.Date("2012-01-01","%Y-%m-%d"),
              max = as.Date("2017-03-06","%Y-%m-%d"),
              value = c(as.Date("2016-03-06","%Y-%m-%d"),
<<<<<<< HEAD
                       as.Date ("2017-03-06","%Y-%m-%d")),
=======
                        as.Date("2017-03-06","%Y-%m-%d")),
>>>>>>> 78cb515bfee18cefcc8faa3247c7270248b60ea7
              timeFormat = "%Y-%m-%d")
)
