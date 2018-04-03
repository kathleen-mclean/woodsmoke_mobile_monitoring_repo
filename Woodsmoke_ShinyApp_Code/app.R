

library(shiny)
library(sp)
library(raster)
library(maptools)
library(rgdal)
library(htmlwidgets)
library(GISTools)
library(ggmap)
library(RColorBrewer)
library(hexbin)
library(Cairo)
library(grid)
library(gridExtra)
library(tidyverse)
library(stringr)
library(lubridate)
select <- dplyr::select

# Load the Neph to PM2.5 conversions data
load("data/Neph.to.BAM.PM25.Conversions.rdata")

# Define UI for the app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Woodsmoke Mobile Monitoring Data Display"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input the data files ----
      conditionalPanel(condition = "input.tabPanels == 1",
                       fileInput("trip_list", "Choose TripList.csv file:",
                                 accept = c("text/csv", "text/comma-saparated-values, text/plain", ".csv")),
                       numericInput("fixed_site_lat", "Enter the latitude of the fixed site monitor in decimal degrees:", 0),
                       numericInput("fixed_site_long", "Enter the longitude of the fixed site monitor in decimal degrees:", 0),
                       
                       fileInput("AETH_data", 
                                 "Choose all Aethalometer .dat or .txt files:",
                                 multiple = T,
                                 accept = c("text/plain", ".txt", ".dat")),
                       
                       checkboxInput("include_NEPH", "Include Nephelometer data?", value = F),
                       uiOutput("NEPH_conditionalInput"),
                       
                       checkboxInput("min_lat", "Remove data with latitude smaller than:", value = F),
                       uiOutput("min_lat_conditionalInput"),
                       checkboxInput("max_lat", "Remove data with latitude larger than:", value = F),
                       uiOutput("max_lat_conditionalInput"),
                       checkboxInput("min_long", "Remove data with longitude smaller than:", value = F),
                       uiOutput("min_long_conditionalInput"),
                       checkboxInput("max_long", "Remove data with longitude larger than:", value = F),
                       uiOutput("max_long_conditionalInput")
      ),
      conditionalPanel(condition = "input.tabPanels == 2",
                       radioButtons("mapchoice",
                                    "Map for:", 
                                    choiceNames = c("All trips", "Night trips"),
                                    choiceValues = c("all", "night")),
                       uiOutput("NEPH_conditional_varchoice"),
                       radioButtons("mapzoom",
                                    "Choose zoom level of the map tiles:",
                                    choices = 11:13,
                                    selected = 12),
                       downloadButton('downloadMap', "Download map")
                       )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Add a tabset
      tabsetPanel(
        tabPanel("Documentation", value = 1,
                 h2("These instructions describe how to use this application:"),
                 p("1. Enter the TripList.csv file. This file should have 1 row per trip and provides summary information about the trip. The Trip List file must have column headings: Trip, Date, Community, Night/Day, Code, Start, End, Direction, DayOfWeek"),
                 p("2. Enter the latitude and longitude of the fixed site monitor, in decimal degrees. If there is no fixed site monitor, leave the latitude and longitude values as 0."),
                 p("3. Click 'Browse' and select all of the Aethalometer .dat or .txt files. The file names of input Aethalometer .dat or .txt files must have the following format: Trip1_AE33.dat"),
                 p("4. If Nephelometer data are available, check the box. Then click 'Browse' and select all of the Nephelometer .txt files. The file names of input Nephelometer .txt files must have the following format: Trip1_NEPH.txt"),
                 p("5. If there is some data you wish to exclude from the map based on latitude and longitude, check the appropriate box(es) and enter the value(s) in decimal degrees. If not, leave the values as 0."),
                 p("6. Once all of the files have uploaded, click over to the 'Maps' tab. Please be patient while the map loads.")
                 
                 ),
        tabPanel("Maps", value = 2,
                 # Add a conditional panel so that nothing here will show unless
                 # all necessary files have been loaded above
                 conditionalPanel(
                   condition = "output.filesUploaded == true",
                   
                   p("For each trip, the values from the instruments were converted to Z scores which compares each value to the average of that trip (e.g. a Z score of 0 means that value was equal to the mean of that trip, a Z score of 1 means the value was one standard deviation higher than the mean, and a Z score of -1.5 means the value was one and a half standard deviations less than the mean). The Z scores from all trips were then averaged across the route and this is what is shown in the map. Because these Z scores are calculated on an exponential scale, we show Delta C or PM2.5 concentration estimates on the legend corresponding to what these average Z scores were roughly equal to across the monitoring hours. This is to add context to the shading and make it easier to understand, but remember these are just estimates and are intended to be used to compare two spots on the map rather than taking them as a hard value."),
                   
                   plotOutput("current_map")
                   
                 )),
        id = "tabPanels")
    )
  )
)

# Define server logic ----
server <- function(input, output) {
  
  # Increase the size limit for file upload to 30 MB (default is 5 MB)
  options(shiny.maxRequestSize = 30*1024^2)
  
  # Load reactive conductors/functions stored in separate script files
  source("reactive_load_clean.R", local = TRUE)
  source("reactive_spatial.R", local = TRUE)
  source("func_clean.R", local = TRUE)
  source("reactive_conversions.R", local = TRUE)
  
  # This function sets the condition for the conditional panel
  # If fileUploaded == true then the additional panels on the UI will show
  output$filesUploaded <- reactive({
    if(input$include_NEPH){
      test <- all(!is.null(in_trip_list()), 
                  !is.null(in_aeth_data()), !is.null(in_neph_data()))
    } else{
      test <- all(!is.null(in_trip_list()), 
                  !is.null(in_aeth_data()))
    }
    return(test)
  })
  outputOptions(output, 'filesUploaded', suspendWhenHidden = FALSE)
  
  output$NEPH_conditionalInput <- renderUI({
    if(input$include_NEPH){
      fileInput("NEPH_data", 
                "Choose all Nethelometer .txt files:",
                multiple = T,
                accept = c("text/plain", ".txt"))
    }
  })
  
  output$NEPH_conditional_varchoice <- renderUI({
    if(input$include_NEPH){
      radioButtons("varchoice",
                   "Variable to map:",
                   choiceNames = c("PM2.5", "Delta C"),
                   choiceValues = c("Z.BS.log", "Z.DC.log"))
    }
  })
  
  output$min_lat_conditionalInput <- renderUI({
    if(input$min_lat){
      numericInput("min_lat_num", "Enter the latitude in decimal degrees:", 0)
    }
  })
  
  output$max_lat_conditionalInput <- renderUI({
    if(input$max_lat){
      numericInput("max_lat_num", "Enter the latitude in decimal degrees:", 0)
    }
  })
  
  output$min_long_conditionalInput <- renderUI({
    if(input$min_long){
      numericInput("min_long_num", "Enter the longitude in decimal degrees:", 0)
    }
  })
  
  output$max_long_conditionalInput <- renderUI({
    if(input$max_long){
      numericInput("max_long_num", "Enter the longitude in decimal degrees:", 0)
    }
  })
  
  output$current_map <- renderPlot({
    
    variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
    print(trip_map())
    grid.text(label = ifelse(variable == "Z.DC.log", 
                             "This map shows the average spatial patterns captured by an Aethalometer \nwhich measures a signal specific to woodsmoke called 'Delta C'.",
                             "This map shows the average spatial patterns captured by a Nephelometer \nwhich measures an estimate of total PM2.5 levels."), 
              x = unit(0.04, "npc"), y = unit(0.78, "npc"), hjust = 0)
    
  }, width = 1000, height = 1000)
  
  output$downloadMap <- downloadHandler(
    filename = function(){
      paste0(in_trip_list()$Community, 
            "_Map_",
            ifelse(input$varchoice == "Z.DC.log", "DeltaC", "PM25"),
            "_",
            ifelse(input$mapchoice == "night", "NightTrips", "AllTrips"),
            ".pdf")
    },
    
    content = function(file){
      variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
      
      pdf(file, width = 11, height = 8.5)
      print(trip_map())
      grid.text(label = ifelse(variable == "Z.DC.log", 
                               "This map shows the average spatial patterns captured by an Aethalometer \nwhich measures a signal specific to woodsmoke called 'Delta C'.",
                               "This map shows the average spatial patterns captured by a Nephelometer \nwhich measures an estimate of total PM2.5 levels."), 
                x = unit(0.04, "npc"), y = unit(0.78, "npc"), hjust = 0)
      dev.off()
    }
  )
  
}

shinyApp(ui = ui, server = server)
