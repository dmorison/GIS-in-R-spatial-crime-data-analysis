library(shiny)
libs <- c("ggplot2", "rgeos", "rgdal", "maps", "mapdata", "mapproj", "maptools", "sp")
lapply(libs, library, character.only = TRUE)
# change directory when publishing app
crimes <- read.csv("../../Data/crimes-2016-types.csv")
dir_1 <- "../../References/Creating-maps-in-R-master/Creating-maps-in-R-master/data/"
ldnMap <- readOGR(file.path(dir_1), layer = "london_sport")
proj4string(ldnMap) <- CRS("+init=epsg:27700")
ldnMap.wgs84 <- spTransform(ldnMap, CRS("+init=epsg:4326"))

function(input, output) {
  
  getData <- reactive({
    crimeTypesActual <- levels(crimes$Crime.type)
    crimeTypesActive <- c(input$`Anti-social_behaviour`, input$Bicycle_theft, input$Burglary,
                          input$Criminal_damage_and_arson, input$Drugs, input$Other_crime, input$Other_theft,
                          input$Possession_of_weapons, input$Public_order, input$Robbery, input$Shoplifting,
                          input$Theft_from_the_person, input$Vehicle_crime, input$Violence_and_sexual_offences)
    
    crimeTypes <- crimeTypesActual[crimeTypesActive]
    plotData <- crimes[which(crimes$Crime.type %in% crimeTypes), ]
    return(plotData)
  })
  
  output$crimeMap <- renderPlot({
    
    mapData <- getData()
    mapData <- mapData[which(mapData$Month == input$month), ]
    
    ggplot(ldnMap.wgs84) + geom_polygon(aes(x = long, y = lat, group = group), fill = "white", colour = "black") + 
      geom_point(data = mapData, aes(x = Longitude, y = Latitude, colour = Crime.type)) +
      scale_colour_manual(values = rainbow(14)) +
      theme(text = element_text(size = 14, face = "bold")) +
      labs(title = "Crime incidences for each month of 2016 in London")
    
  })
  
  output$crimeTrend <- renderPlot({
    
    lineData <- getData()
    lineData <- aggregate(lineData["Crime.type"], by = lineData[c("Month", "Crime.type")], FUN = length)
    colnames(lineData)[3] <- "Total"
    monthNames <- c("January","February","March","April","May","June","July",
                    "August","September","October","November","December")
    
    ggplot(lineData) +
      geom_line(aes(x = Month, y = Total, group = Crime.type, colour = Crime.type), size = 2) +
      scale_x_discrete(labels = monthNames) +
      scale_colour_manual(values = rainbow(14)) +
      theme(text = element_text(size = 14, face = "bold"),
            axis.text.x = element_text(angle = 60, hjust = 1)) +
      labs(title = "Monthly crime totals for each month of 2016 in London")
    
  })
    
}