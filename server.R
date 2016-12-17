library(shiny)
library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(sqldf)
library(ggmap)
library(rgdal)
library(scales)
library(ggthemes)
library(leaflet)
require(leaflet)


boston_map <- get_map(location = "Mass Ave/Albany st.", zoom = 12)
load("boston_map.Rdata")
load("mappolice.RData")
load("map311.RData")
load("map911.RData")
load("spatial.RData")

# density plot
police_density <- ggmap(boston_map, extent = "device") + geom_density2d(data = police, aes(x = LONG, y = LAT), size = 0.3) + 
  stat_density2d(data = police, 
                 aes(x = LONG, y = LAT, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE)

density311 <- ggmap(boston_map, extent = "device") + geom_density2d(data = data311, aes(x = LONGITUDE, y = LATITUDE), size = 0.3) + 
  stat_density2d(data = data311, 
                 aes(x = LONGITUDE, y = LATITUDE, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) # + ggtitle("Density Plot of Crime Data in Boston")

density911 <- ggmap(boston_map, extent = "device") + geom_density2d(data = data_911, aes(x = Longitude, y = Latitude), size = 0.3) + 
  stat_density2d(data = data_911, 
                 aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                 bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) # + ggtitle("Density Plot of 911 Data in Boston")
density911
#Server body

shinyServer(function(input, output) {
  output$spatial <- renderPlot({
    switch(input$plotType,
           "spatial.density.plot" = police_density)})
  
  output$dot <-  renderLeaflet({
    pal <- colorFactor(c("navy", "red"), domain = c("0", "1"))
    switch(input$plotType,
          "spatial.dot.plot"=leaflet(data = subset(police, eventindex==input$et)) %>% addProviderTiles(input$bg) %>%
               setView( lng = -71.073689, lat = 42.333539,  zoom = 11 )  %>%
               addCircles(~-71.073689, ~42.333539, radius=380, 
                          color="grey", stroke = TRUE, fillOpacity = 0.3) %>%
               addCircles(~LONG, ~LAT, radius = ~ifelse(within.25 == "0", 30,30),  
                          color = ~pal(within.25), stroke = FALSE, fillOpacity = 0.5) 
          )})
  
  output$weather <- renderPlot({
    ggmap(boston_map, extent = "device") + 
      geom_density2d(data = mappolice[mappolice$extremday==input$weather,],aes(x = Longitude, y = Latitude), size = 0.3)+ 
      stat_density2d(data = mappolice[mappolice$extremday==input$weather,], 
                     aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                     bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
      scale_alpha(range = c(0, 0.3), guide = FALSE) 
  })

  #311 density plot
  output$spatial311 <- renderPlot({
    switch(input$plottype,
           "density.plot311" = density311)})
  
  #311 dot plot
    output$dot311 <- renderLeaflet({
      pal <- colorFactor(c("navy", "red"), domain = c("0", "1"))
      switch(input$plottype,
             "dot.plot311"=leaflet(data = subset(data311, eventindex==input$et311)) %>% addProviderTiles(input$bg311) %>%
        setView( lng = -71.073689, lat = 42.333539,  zoom = 11 )  %>%
        addCircles(~-71.073689, ~42.333539, radius=380, 
                   color="grey", stroke = TRUE, fillOpacity = 0.3) %>%
        addCircles(~LONGITUDE, ~LATITUDE, radius = ~ifelse(within.25 == "0", 30, 30),  
                   color = ~pal(within.25), stroke = FALSE, fillOpacity = 0.5) 
      )})
  #311 weather plot
    output$weather311 <- renderPlot({
      ggmap(boston_map, extent = "device") + 
        geom_density2d(data = map311[map311$extremday==input$weather311,],aes(x = Longitude, y = Latitude), size = 0.3)+ 
        stat_density2d(data = map311[map311$extremday==input$weather311,], 
                       aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                       bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0, 0.3), guide = FALSE) 
    })
    
  #911 density plot
    output$spatial911 <- renderPlot({
      switch(input$plottype911,
             "density.plot911" = density911)})
  #911 dot plot
    output$dot911 <- renderLeaflet({
      pal <- colorFactor(c("navy", "red"), domain = c("0", "1"))
      switch(input$plottype911,
             "dot.plot911"=leaflet(data = subset(data_911, eventindex==input$et911)) %>% addProviderTiles(input$bg911) %>%
               setView( lng = -71.073689, lat = 42.333539,  zoom = 11 )  %>%
               addCircles(~-71.073689, ~42.333539, radius=380, 
                          color="grey", stroke = TRUE, fillOpacity = 0.3) %>%
               addCircles(~Longitude, ~Latitude, radius = ~ifelse(within.25 == "0", 30, 30),  
                          color = ~pal(within.25), stroke = FALSE, fillOpacity = 0.5) 
      )})
    
  #911 weather plot
    #output$weather911 <- renderPlot({
      
      #if (input$select.weather911=="high") {wh911}
      #if  (input$select.weather911=="low") {ggmap(boston_map, extent = "device") + geom_density2d(data = d911wc, aes(x = Longitude, y = Latitude), size = 0.3) + 
          #stat_density2d(data = d911wc, 
          #               aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
           #              bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
          #scale_alpha(range = c(0, 0.3), guide = FALSE) + ggtitle("Extreme Cold Weather")}
      #else if  (input$select.weather911=="medium") {wm911}
      #})
    
    output$weather911 <- renderPlot({
      ggmap(boston_map, extent = "device") + 
        geom_density2d(data = map911[map911$extremday==input$weather911,],aes(x = Longitude, y = Latitude), size = 0.3)+ 
        stat_density2d(data = map911[map911$extremday==input$weather911,], 
                       aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..), size = 0.01, 
                       bins = 16, geom = "polygon") + scale_fill_gradient(low = "green", high = "red") + 
        scale_alpha(range = c(0, 0.3), guide = FALSE) 
    })
    
    
  })
  