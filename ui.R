library(shiny)
library(shinythemes)
library(tidyr)
library(dplyr)
library(plyr)
library(readr)
library(ggplot2)
library(sqldf)
library(leaflet)
require(leaflet)

shinyUI(fluidPage(
  theme = shinytheme("flatly"),
  ## title
  titlePanel(title=div(img(src="logo.png",height=80,width=160),
                       "MS in Statistical Practice: Homeless People of Boston")),
  
  ## all panels
  tabsetPanel(              
    ## Home page
    tabPanel(title = "Home Page",img(src="cover2.png",width="100%",height="100%"),
             style = "background-color: #BCC6CC;"
    ),
    
    ## spatial panel      
    tabPanel(title = "Spatial",
             tabsetPanel(
               tabPanel("911",titlePanel("Page's title"),
                                         column(3,wellPanel(radioButtons("plottype911", label = h3("Choose the plot"),
                                                                         choices = list(Density = "density.plot911",
                                                                                        Dot = "dot.plot911",
                                                                                        Weather = "weather911"), 
                                                                         selected = "density.plot911"),
                                                            conditionalPanel(
                                                              condition = "input.plottype911 == 'weather911'",
                                                              selectInput("weather911", label = h5("Choose the weather condition"), 
                                                                          choices = list(Cold = "0", Moderate = "1", Hot = "2"), 
                                                                          selected = "0")
                                                              
                                                            ),
                                                            
                                                            conditionalPanel(
                                                              condition = "input.plottype911 == 'dot.plot911'",
                                                              selectInput("bg911", label = h5("Choose the map type"), 
                                                                          c("Positron" = "CartoDB.Positron",
                                                                            "Open Street Map" = "OpenStreetMap"), 
                                                                          selected = "CartoDB.Positron"),
                                                              selectInput("et911", "Event Types:",
                                                                          c("Request EMS Response" = "1", "Poisoning / Overdose" = "2",
                                                                            "Investigate Odor" = "3","Difficulty Breathing" = "4", 
                                                                            "Patient Refusing Treatment and/or Transport" = "5", "Medical Incident" = "6",
                                                                            "Emotionally Disturbed Person" = "7", "Other" = "8"))
                                                              
                                                              
                                                            )
                                                            
                                         )),
               mainPanel(
                 conditionalPanel(
                   condition = "input.plottype911 == 'density.plot911'",
                   plotOutput('spatial911')
                 ),
                 conditionalPanel(
                   condition = "input.plottype911 == 'dot.plot911'",
                   leafletOutput('dot911')
                 ),
                 conditionalPanel(
                   condition = "input.plottype911 == 'weather911'",
                   plotOutput('weather911')
                 ))),
               
               tabPanel("311",titlePanel("Page's title"),
                        column(3,wellPanel(radioButtons("plottype", label = h3("Choose the plot"),
                                                        choices = list(Density = "density.plot311",
                                                                       Dot = "dot.plot311",
                                                                       Weather = "weather311"), 
                                                        selected = "density.plot311"),
                                           conditionalPanel(
                                             condition = "input.plottype == 'weather311'",
                                             selectInput("weather311", label = h5("Choose the weather condition"), 
                                                         choices = list(Cold = "0", Moderate = "1", Hot = "2"), 
                                                         selected = "0")
                                             
                                           ),
                                           
                                           conditionalPanel(
                                             condition = "input.plottype == 'dot.plot311'",
                                             selectInput("bg311", label = h5("Choose the map type"), 
                                                         c("Positron" = "CartoDB.Positron",
                                                           "Open Street Map" = "OpenStreetMap"), 
                                                         selected = "CartoDB.Positron"),
                                             selectInput("et311", "Event Types:",
                                                         c("Requests for Street Cleaning" = "1", "Graffiti Removal" = "2",
                                                           "Improper Storage of Trash (Barrels)" = "3","Empty Litter Basket" = "4", 
                                                           "Needle Pickup" = "5", "Illegal Occupancy" = "6",
                                                           "Trash on Vacant Lot" = "7", "Overflowing or Un-kept Dumpster" = "8", 
                                                           "Request for Litter Basket Installation" = "09", "Litter Basket Maintenance" = "10", 
                                                           "Construction Debris" = "11"))
                                             
                                             
                                           )
                                           
                        )),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.plottype == 'density.plot311'",
                            plotOutput('spatial311')
                          ),
                          conditionalPanel(
                            condition = "input.plottype == 'dot.plot311'",
                            leafletOutput('dot311')
                          ),
                          conditionalPanel(
                            condition = "input.plottype == 'weather311'",
                            plotOutput('weather311')
                          )
                        )),
                        
               
               
               tabPanel("Police",titlePanel("Page's title"),
                        column(3,wellPanel(radioButtons("plotType", label = h3("Choose the plot"),
                                                        choices = list(Density = "spatial.density.plot",
                                                                       Dot = "spatial.dot.plot",
                                                                       Weather = "spatial.weather"), 
                                                        selected = "spatial.density.plot"),
                                           conditionalPanel(
                                             condition = "input.plotType == 'spatial.weather'",
                                             selectInput("weather", label = h5("Choose the weather condition"), 
                                                         choices = list(Cold = "0", Moderate = "1", Hot = "2"), 
                                                         selected = "0")
                                             
                                           ),
                                           
                                           conditionalPanel(
                                             condition = "input.plotType == 'spatial.dot.plot'",
                                             selectInput("bg", label = h5("Choose the map type"), 
                                                         c("Positron" = "CartoDB.Positron",
                                                           "Open Street Map" = "OpenStreetMap"), 
                                                         selected = F),
                                             selectInput("et", "Event Types:",
                                                         c("Assault" = "1", "Assembly or Gathering Violations" = "2",
                                                           "Disorderly Conduct" = "3","Drug Violation" = "4", 
                                                           "Harassment" = "5", "Liquor Violation" = "6",
                                                           "Medical Assistance" = "7", "Property Damage" = "8", 
                                                           "Prostitution" = "9", "Vandalism" = "10", 
                                                           "Verbal Disputes" = "11"))
                                            
                                             
                                           )
                  
                        )),
                        mainPanel(
                          conditionalPanel(
                            condition = "input.plotType == 'spatial.density.plot'",
                            plotOutput('spatial')
                          ),
                          conditionalPanel(
                            condition = "input.plotType == 'spatial.dot.plot'",
                            leafletOutput('dot')
                          ),
                          conditionalPanel(
                            condition = "input.plotType == 'spatial.weather'",
                            plotOutput('weather')
                          )
                        )
                      ))),
    
    ## temporal panel
    tabPanel(title = "Temporal",
             tabsetPanel(
               tabPanel("911",titlePanel("add this page's title"),
                        column(3,wellPanel(selectInput("9",
                                                       "911 data",choices = c("a","b",selected="a")),
                                           checkboxGroupInput("radio","title",
                                                              c("a","b","c","d"),inline = T)
                                           
                        ))
               ),
               
               tabPanel("311"),
               
               tabPanel("Police")
               
             ),
             plotOutput("Weather"))
    
  )))
