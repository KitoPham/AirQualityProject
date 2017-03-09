library(shiny)
my.ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      tableOutput("selected"),
      tableOutput("selectedLocation")
      
    ),
    mainPanel(
      plotOutput("plot", click = "point_click", 
                 brush = brushOpts(
                      id = "zoom_brush",
                      resetOnNew = TRUE
                      )
      ),
      
      column(width = 8,
             plotOutput("zoomed", click = "point_click2")
             ),
      
      sliderInput("date","Date:",
                  min = as.Date("2017-02-07","%Y-%m-%d"),
                  max = as.Date("2017-03-07","%Y-%m-%d"),
                  value = c(as.Date("2016-03-01","%Y-%m-%d"),
                            as.Date("2017-03-07","%Y-%m-%d")),
                  
                  timeFormat = "%Y-%m-%d"),
      plotOutput("overtime", hover = "overtimehover"),
      
      plotOutput("barCompare", hover = "barHover")
    )
  )
  
  
  
)

