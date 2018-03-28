

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
library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(ggplot2)
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
                       helpText("The Trip List file must have column headings:"),
                       helpText("Trip, Date, Community, Night/Day, Code, ON, Start, End, OFF, Direction, DOW"),
                       checkboxInput("min_lat", "Remove data with latitude smaller than:", value = F),
                       uiOutput("min_lat_conditionalInput"),
                       checkboxInput("max_lat", "Remove data with latitude larger than:", value = F),
                       uiOutput("max_lat_conditionalInput"),
                       checkboxInput("min_long", "Remove data with longitude smaller than:", value = F),
                       uiOutput("min_long_conditionalInput"),
                       checkboxInput("max_long", "Remove data with longitude larger than:", value = F),
                       uiOutput("max_long_conditionalInput"),
                       fileInput("GPS_data", 
                                 "Choose all GPS .csv files:",
                                 multiple = T,
                                 accept = c("text/csv", "text/comma-saparated-values, text/plain", ".csv")),
                       helpText("The file names of input GPS .csv files must have the following format: Trip1_GPS.csv"),
                       helpText("The input GPS .csv files must have column headings:"),
                       helpText("Date, Time, Latitude, Longitude, Speed(km/hour)"),
                       
                       fileInput("AETH_data", 
                                 "Choose all Aethalometer .txt files:",
                                 multiple = T,
                                 accept = c("text/plain", ".txt")),
                       helpText("The file names of input Aethalometer .txt files must have the following format: Trip1_AE33.csv"),
                       
                       checkboxInput("include_NEPH", "Include Nephelometer data?", value = F),
                       uiOutput("NEPH_conditionalInput"),
                       uiOutput("NEPH_conditionalInput2")    
      ),
      conditionalPanel(condition = "input.tabPanels == 2",
                       radioButtons("mapchoice",
                                    "Map for:", 
                                    choiceNames = c("All trips", "Night trips"),
                                    choiceValues = c("all", "night")),
                       uiOutput("NEPH_conditional_varchoice"),
                       numericInput("fixed_site_lat", "Enter the latitude of the fixed site monitor in decimal degrees:", 0),
                       numericInput("fixed_site_long", "Enter the longitude of the fixed site monitor in decimal degrees:", 0),
                       downloadButton('downloadMap', "Download map")
                       )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Add a tabset
      tabsetPanel(
        tabPanel("Input Data Preview", value = 1,
                 h5("The first six rows of the Trip List input dataset are shown below:"),
                 dataTableOutput("inputtriplist"),
                 
                 h5("The first six rows of the GPS input dataset are shown below:"),
                 dataTableOutput("inputgps"),
                 
                 h5("The first six rows of the Aethalometer input dataset are shown below:"),
                 dataTableOutput("inputaeth"),
                 
                 uiOutput("NEPH_conditionalInput_table"),
                 dataTableOutput("inputneph")),
        tabPanel("Maps", value = 2,
                 # Add a conditional panel so that nothing here will show unless
                 # all necessary files have been loaded above
                 conditionalPanel(
                   condition = "output.filesUploaded == true",
                   
                   # textOutput("temp_text"),
                   plotOutput("temp_plot")
                   
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
      test <- all(!is.null(in_trip_list()), !is.null(in_gps_data()), 
                  !is.null(in_aeth_data()), !is.null(in_neph_data()))
    } else{
      test <- all(!is.null(in_trip_list()), !is.null(in_gps_data()), 
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
  
  output$NEPH_conditionalInput2 <- renderUI({
    if(input$include_NEPH){
      helpText("The file names of input Nephelometer .txt files must have the following format: Trip1_Aurora.csv")
    }
  })
  
  output$NEPH_conditional_varchoice <- renderUI({
    if(input$include_NEPH){
      radioButtons("varchoice",
                   "Variable to map:",
                   choiceNames = c("BS", "DC"),
                   choiceValues = c("Z.BS.log", "Z.DC.log"))
    }
  })
  
  output$NEPH_conditionalInput_table <- renderUI({
    if(input$include_NEPH){
      h5("The first six rows of the Nephelometer input dataset are shown below:")
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
  
  # Show the head of the data files in the first tab panel
  output$inputtriplist <- renderDataTable({
    
    head(in_trip_list())
  })
  
  output$inputgps <- renderDataTable({
    
    head(in_gps_data())
  })
  
  output$inputaeth <- renderDataTable({
    
    head(in_aeth_data())
  })
  
  output$inputneph <- renderDataTable({
    
    head(in_neph_data())
  })
  
  output$temp_plot <- renderPlot({
    
    print(trip_map())
    
  }, width = 1000, height = 1000)
  # 
  # output$temp_text <- renderText({
  #   
  #   summary(trip_polygons()$Z_binned)
  #   
  # })
  
  output$downloadMap <- downloadHandler(
    filename = function(){
      paste0(in_trip_list()$Community, 
            "_Map_",
            ifelse(input$varchoice == "Z.DC.log", "DeltaC", "Bscat"),
            "_",
            ifelse(input$mapchoice == "night", "Night", "All"),
            ".pdf")
    },
    
    content = function(file){
      pdf(file)
      print(trip_map())
      dev.off()
    }
  )
  
}

shinyApp(ui = ui, server = server)
