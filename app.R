source('myUi.R')
source('myServer.R')

shinyApp(ui = my.ui, server = my.server)
