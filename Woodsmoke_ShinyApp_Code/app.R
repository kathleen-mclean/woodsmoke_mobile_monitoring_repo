

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
library(leaflet)
library(mapview)
select <- dplyr::select
extract <- raster::extract

# Load the Neph to PM2.5 conversions data
load("data/Neph.to.BAM.PM25.Conversions.rdata")

# Define UI for the app ----
ui <- fluidPage(
  
  # change CSS for the progress bar location and size
  tags$head(
    tags$style(
      HTML(".shiny-notification {
           height: 100px;
           width: 800px;
           position:fixed;
           top: calc(50% - 50px);;
           left: calc(60% - 400px);;
           }
           "
      )
      )
      ),
  
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
                       
                       fileInput("AETH_data", 
                                 "Choose all Aethalometer .dat or .txt files:",
                                 multiple = T,
                                 accept = c("text/plain", ".txt", ".dat")),
                       
                       checkboxInput("include_NEPH", "Include Nephelometer data?", value = F),
                       uiOutput("NEPH_conditionalInput"),
                       
                       checkboxInput("include_fixed_site", "Add the location of a fixed site monitor?", value = F),
                       uiOutput("fixed_site_conditionalInput_lat"),
                       uiOutput("fixed_site_conditionalInput_long"),
                       
                       checkboxInput("min_lat", "Remove data with latitude smaller than:", value = F),
                       uiOutput("min_lat_conditionalInput"),
                       checkboxInput("max_lat", "Remove data with latitude larger than:", value = F),
                       uiOutput("max_lat_conditionalInput"),
                       checkboxInput("min_long", "Remove data with longitude smaller than:", value = F),
                       uiOutput("min_long_conditionalInput"),
                       checkboxInput("max_long", "Remove data with longitude larger than:", value = F),
                       uiOutput("max_long_conditionalInput")
      ),
      # Left side panel on the Maps tab
      conditionalPanel(condition = "input.tabPanels == 2",
                       radioButtons("mapchoice",
                                    "Map for:", 
                                    choiceNames = c("All trips", "Night trips"),
                                    choiceValues = c("all", "night")),
                       uiOutput("NEPH_conditional_varchoice"),
                       radioButtons("mapzoom",
                                    "Choose zoom level of the map tiles:",
                                    choices = 12:14,
                                    selected = 13),
                       downloadButton('downloadPNGMap', "Download map (.png format)"),
                       p(""),
                       downloadButton('downloadPDFMap', "Download map (.pdf format)"),
                       p("It will take several seconds after you click the download button for something to happen...")
                       )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Add a tabset
      tabsetPanel(
        tabPanel("Documentation", value = 1,
                 h2("These instructions describe how to use this application:"),
                 p("1. Enter the TripList.csv file. This file should have 1 row per trip and provides summary information about the trip. The Trip List file must have column headings: Trip, Date, Community, Night/Day, Code, Start, End, Direction, DayOfWeek. The Date column must be formatted as MM/DD/YYYY. If Excel reformats your date column automatically, use an apostrophe before the date to prevent this: i.e. 'MM/DD/YYYY, or use a text editor like Notepad to edit the dates."),
                 p("2. Click 'Browse' and select all of the Aethalometer .dat or .txt files. The file names of input Aethalometer .dat or .txt files must have the following format: Trip1_AE33_YYYYMMDD.dat where YYYYMMDD is a date."),
                 p("3. If Nephelometer data are available, check the box. Then click 'Browse' and select all of the Nephelometer .txt files. The file names of input Nephelometer .txt files must have the following format: Trip1_NEPH_YYYYMMDD.txt where YYYYMMDD is a date."),
                 p("4. Check the box if you want to add the location of a fixed site monitor to the map. Then, enter the latitude and longitude of the fixed site monitor, in decimal degrees with 4+ decimal places."),
                 p("5. If there is some data you wish to exclude from the map based on latitude and longitude, check the appropriate box(es) and enter the value(s) in decimal degrees. If not, leave the values as 0."),
                 p("6. Once all of the files have uploaded, click over to the 'Maps' tab. Please be patient while the map loads."),
                 
                 h2("Input data check:"),
                 
                 h5("Trip List:"),
                 textOutput("inputtriplist"),
                 
                 h5("Aethalometer dataset:"),
                 textOutput("inputaeth"),
                 
                 uiOutput("NEPH_conditionalInput_text"),
                 textOutput("inputneph"),
                 
                 uiOutput("fs_lat_conditionalInput_text"),
                 textOutput("inputfslat"),
                 
                 uiOutput("fs_long_conditionalInput_text"),
                 textOutput("inputfslong")
                 
                 ),
        tabPanel("Maps", value = 2,
                 # Add a conditional panel so that nothing here will show unless
                 # all necessary files have been loaded above
                 conditionalPanel(
                   condition = "output.filesUploaded == true",
                   
                   p("For each trip, the values from the instruments were converted to Z scores which compares each value to the average of that trip (e.g. a Z score of 0 means that value was equal to the mean of that trip, a Z score of 1 means the value was one standard deviation higher than the mean, and a Z score of -1.5 means the value was one and a half standard deviations less than the mean). The Z scores from all trips were then averaged across the route and this is what is shown in the map. Because these Z scores are calculated on an exponential scale, we show Delta C or PM2.5 concentration estimates on the legend corresponding to what these average Z scores were roughly equal to across the monitoring hours. This is to add context to the shading and make it easier to understand, but remember these are just estimates and are intended to be used to compare two spots on the map rather than taking them as a hard value."),
                   
                   # plotOutput("map")
                   leafletOutput("map", height = 750)
                   
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
  
  output$NEPH_conditionalInput_text <- renderUI({
    if(input$include_NEPH){
      h5("Nephelometer dataset:")
    }
  })
  
  output$fs_lat_conditionalInput_text <- renderUI({
    if(input$include_fixed_site){
      h5("Fixed site latitude:")
    }
  })
  
  output$fs_long_conditionalInput_text <- renderUI({
    if(input$include_fixed_site){
      h5("Fixed site longitude:")
    }
  })
  
  output$fixed_site_conditionalInput_lat <- renderUI({
    if(input$include_fixed_site){
      numericInput("fixed_site_lat", "Enter the latitude of the fixed site monitor in decimal degrees (with 4+ decimal places):", 0)
    }
  })
  
  output$fixed_site_conditionalInput_long <- renderUI({
    if(input$include_fixed_site){
      numericInput("fixed_site_long", "Enter the longitude of the fixed site monitor in decimal degrees (with 4+ decimal places):", 0)
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
  
  output$map <- renderLeaflet({
    
    withProgress(message = "Map loading", value = 0.5, {

          currentmap <- leaflet_trip_map()

        })
    
  })
  
  output$downloadPNGMap <- downloadHandler(
    filename = function(){
      paste0(in_trip_list()$Community, 
            "_Map_",
            ifelse(input$varchoice == "Z.DC.log" | is.null(input$varchoice), "DeltaC", "PM25"),
            "_",
            ifelse(input$mapchoice == "night", "NightTrips", "AllTrips"),
            ".png")
    },
    
    content = function(file){
      
      mapshot(x = user_leaflet_map(),
              file = file)
      
    }
  )
  
  output$downloadPDFMap <- downloadHandler(
    filename = function(){
      paste0(in_trip_list()$Community, 
             "_Map_",
             ifelse(input$varchoice == "Z.DC.log" | is.null(input$varchoice), "DeltaC", "PM25"),
             "_",
             ifelse(input$mapchoice == "night", "NightTrips", "AllTrips"),
             ".pdf")
    },
    
    content = function(file){
      
      mapshot(x = user_leaflet_map(),
              file = file)
      
    }
  )
  
  # Input data checks
  output$inputtriplist <- renderText({
    if("data.frame" %in% class(in_trip_list())){
      "Uploaded correctly."
    }
  })
  output$inputaeth <- renderText({
    if("data.frame" %in% class(in_aeth_data())){
      "Uploaded correctly."
    }
  })
  output$inputneph <- renderText({
    if("data.frame" %in% class(in_neph_data())){
      "Uploaded correctly."
    }
  })
  output$inputfslat <- renderText({
    if("numeric" %in% class(in_fs_lat())){
      "Uploaded correctly and located within the data range."
    }
  })
  output$inputfslong <- renderText({
    if("numeric" %in% class(in_fs_long())){
      "Uploaded correctly and located within the data range."
    }
  })

}

shinyApp(ui = ui, server = server)

