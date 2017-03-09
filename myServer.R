

my.server<-function(input,output){
  
  #Loads up csvs
  withProgress(message = 'Loading data csv files', value = 0, {
    feb.7.10 <- read.csv('data/Feb7-10.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb7-10")
    Sys.sleep(0.1)
    feb.11.15 <- read.csv('data/Feb11-15.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb11-15")
    Sys.sleep(0.1)
    feb.16.20 <- read.csv('data/Feb16-20.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb16-20")
    Sys.sleep(0.1)
    feb.21.24 <- read.csv('data/Feb21-24.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb21-24")
    Sys.sleep(0.1)
    feb.25.26 <- read.csv('data/Feb25-26.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb25-26")
    Sys.sleep(0.1)
    feb.27.28 <- read.csv('data/Feb27-28.csv', stringsAsFactors=FALSE)
    incProgress(.10, detail = "Loaded Feb27-28")
    Sys.sleep(0.1)
    mar.1.4 <- read.csv('data/Mar1-4.csv', stringsAsFactors=FALSE)
    incProgress(.20, detail = "Loaded Mar1-4")
    Sys.sleep(0.1)
    mar.5.7 <- read.csv('data/Mar5-7.csv', stringsAsFactors=FALSE)
    incProgress(.20, detail = "Loaded Mar5-7")
    Sys.sleep(0.1)
  })
  
  # Stores all CSV files that contains dats from Feb 7 to March 7 into a dataframe
  us.all.frame <- rbind(mar.5.7, mar.1.4, feb.27.28, feb.25.26,
                      feb.21.24, feb.16.20, feb.11.15, feb.7.10)
  
  us.reactive <- reactive({
    withProgress(message = 'Filtering data', value = 0, {
      
      # Filters for values lower then the minimum and maximum utc(date) in the dataframe 
      # us.all.frame
      frame <- filter(us.all.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
                filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z")))
      incProgress(.30, detail = "Dates filtered")
      
      # Groups location and finds the mean of the values within 'frame'
      frame <- aggregate(value~location + latitude + longitude + city, frame, mean)
      incProgress(.60, detail = "Averaging location data")
      
      incProgress(.10, detail = "Pruning data")
    })

    
    #frame$value = cut(frame$value, 10, labels = c('0 to 3', "3 to 6", "6 to 8", "8 to 10", "10 to 13","13 to 16", "16 to 19", "19 to 22", "22 to 27", "NA"))
    return(frame)
  })
  
  output$selectthis <- renderUI({
    selectizeInput("city1", "City 1", unique(us.all.frame$city), options = list(maxItems = 1))
  }) 
  output$selectthat <- renderUI({
    selectizeInput("city2", "City 2", unique(us.all.frame$city), options = list(maxItems = 1))
  })
  #updateSelectInput(session, "selectthis", choices = unique(us.frame$location))
  #updateSelectInput(session, "selectthat", choices = unique(us.frame$location))
    
                                                                                                 
  

states <- map_data("state")
ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  ##Plots World Map
  output$plot<-renderPlot({
    
  # Filters out Hawaii and Alaska
    frame <- filter(us.reactive(), latitude > 25) %>% 
      filter(latitude < 55)
  
    plot <- ggplot() + 
        geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
      geom_point(data = frame, aes(x = longitude, y = latitude, color = value, size = value)) +
      scale_color_gradient(low = "White", high = "red") +
      labs(title = "Particle Pollution Within the US",  # plot title
           x = "Longitude",  # x-axis label (with units!)
           y = "Latitude",  # y-axis label (with units!)
           color = "Amount of µg/m³",  # legend label for the "color" property
          size = "µg/m³ Size")+
      theme(plot.title = element_text(face = 'bold', size = 14))
  
  return(plot)
  })
  
  
  ##Zoomed in Plot
  output$zoomed <- renderPlot({
  
    # Filters out Hawaii and Alaska
    frame <- filter(us.reactive(), latitude > 25) %>% 
      filter(latitude < 55)  
    
    plot <- ggplot() + 
        geom_polygon(data = states, fill = "#629632", color = "#000000", aes(x = long, y = lat, group = group))  + 
        geom_point(data = frame, aes(x = longitude, y = latitude, color = value, size = value)) +
      scale_color_gradient(low = "White", high = "red") +
        coord_cartesian(xlim = ranges2$x, ylim = ranges2$y) +
      labs(title = "Particle Pollution Within the US (Zoom)",  # plot title
           x = "Longitude",  # x-axis label (with units!)
           y = "Latitude",  # y-axis label (with units!)
           color = "Amount of µg/m³",  # legend label for the "color" property
           size = "µg/m³ Size") +
      theme(plot.title = element_text(face = 'bold', size = 14))
      
    
    return(plot)
  })
  
  ##Check for zoom range
  observe({
    brush <- input$zoom_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  #set up for brush
  selecttable <- reactiveValues(
    row = data.frame()
  )
  
  # Filters for the city picked within the 'us.all.frame' and plots a graph of the two values
  # from the cities
  output$overtime <- renderPlotly({
    plot <- ggplot()
    if (!is.null(input$city1)&&!is.null(input$city2)){
      city1.frame <- filter(us.all.frame, city == input$city1);
      city1.frame <- filter(city1.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
                      filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z"))) %>% 
                        group_by(city, Date = as.Date(utc)) %>% 
                          summarize(value = mean(value))
      
      city2.frame <- filter(us.all.frame, city == input$city2);
      city2.frame <- filter(city2.frame, as.Date(utc) < as.Date(paste(input$date[2],"T08:00:00.000Z"))) %>% 
        filter(as.Date(utc) > as.Date(paste(input$date[1],"T08:00:00.000Z"))) %>% 
                      group_by(city, Date = as.Date(utc)) %>% 
                        summarize(value = mean(value))
      
      cityframe <- rbind(city1.frame, city2.frame)
      
      # plots scatter plot of average measurements per day of pm25
      plot <- ggplot() +
        geom_point(data = cityframe, mapping = aes(x = Date, y = value, color = city))
        labs(title = "PM2.5 Comparison Between Cities",  # plot title
             x = "Date",  # x-axis label (with units!)
             y = "µg/m³") +  # y-axis label (with units!)
        theme(plot.title = element_text(face = 'bold', size = 14))
      
    }
    return(ggplotly(plot))
  })
  
  # Creates a bar chart to compare city1 and city2 
  output$barCompare <- renderPlot({
    if (!is.null(input$city1)&&!is.null(input$city2)){
      city1.frame <- filter(us.reactive(), city == input$city1) %>% 
                      group_by(city) %>% 
                        summarize(average = mean(value))
      city2.frame <- filter(us.reactive(), city == input$city2)%>% 
                      group_by(city) %>% 
                        summarize(average = mean(value))
      cityframe <- rbind(city1.frame, city2.frame)
      
      #Plots bar chart comparing city average data
      plot <- ggplot() +
        geom_col(data = cityframe, mapping = aes(x = city, y = average, fill=city)) +
        labs(title = "PM2.5 Comparison Between Cities",  # plot title
             x = "City",  # x-axis label (with units!)
             y = "µg/m³") + # y-axis label (with units!)
        theme(plot.title = element_text(face = 'bold', size = 14))
      
      return(plot)
    }
  })
}



























