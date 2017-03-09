library(shiny)
my.ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      uiOutput("selectthis"),
      uiOutput("selectthat"),
      sliderInput("date","Date:", 
                  min = as.Date("2017-02-07","%Y-%m-%d"),
                  max = as.Date("2017-03-07","%Y-%m-%d"),
                  value = c(as.Date("2016-03-01","%Y-%m-%d"),
                            as.Date("2017-03-07","%Y-%m-%d")),
                  timeFormat = "%Y-%m-%d")
      
    ),
    mainPanel(
      tabsetPanel(type="tabs",
        tabPanel(
          "Compare",
          plotOutput("overtime", hover = "overtimehover"),
          plotOutput("barCompare", hover = "barHover")
        ),
        
        tabPanel(
          "Map",
          plotOutput("plot", 
                     brush = brushOpts(id = "zoom_brush",resetOnNew = TRUE)
                     ),
          
          plotOutput("zoomed")
        )
      )
    )
  )
)


