# These reactive conductors load and clean the data input to the fileInputs on the UI

  in_trip_list <- reactive({
    inFile <- input$trip_list
    
    req(inFile)
    
    trip_data <- read_csv(file = inFile$datapath, 
                          col_types = cols(Date = col_date(format = "%m/%d/%Y")))
    
    validate(
      need(all(c("Date", "Community", "Night/Day", "Start", "End") %in% names(trip_data)) != F, 
           "Trip list .csv file does not have the necessary columns."),
      need(all(grepl("20[0-9][0-9]-[0-9][0-9]-[0-9][0-9]", trip_data$Date)) == T,
           "Check date format in TripList.csv. The Date column must be formatted as MM/DD/YYYY.")
      )
    
    trip_data
    
  })

  in_aeth_data <- reactive({
    inFile <- input$AETH_data
    
    req(inFile)
    validate(
      need(all(unlist(lapply(inFile[["name"]],
                      function(x) grepl(paste0("Trip", "[0-9][0-9]*", "_AE33_", "20[0-9][0-9][0-9][0-9][0-9][0-9]"), x)))) != F,
           "Check that Aethalometer file names are formatted correctly")
    )
    
    aeth_data <- map(1:nrow(inFile), ~ read.table(file = inFile[[.x, "datapath"]],
                                                  header = F, skip = 6, fill = T,
                                                  stringsAsFactors = F))
    names(aeth_data) <- inFile[["name"]]
    
    aeth_data <- aeth_data %>%
      map(setNames, nm = c('Date(yyyy/MM/dd)', 'Time(hh:mm:ss)', 'Timebase', 
                           'RefCh1', 'Sen1Ch1', 'Sen2Ch1', 'RefCh2', 'Sen1Ch2', 'Sen2Ch2', 
                           'RefCh3', 'Sen1Ch3', 'Sen2Ch3', 'RefCh4', 'Sen1Ch4', 'Sen2Ch4', 
                           'RefCh5', 'Sen1Ch5', 'Sen2Ch5', 'RefCh6', 'Sen1Ch6', 'Sen2Ch6', 
                           'RefCh7', 'Sen1Ch7', 'Sen2Ch7', 'Flow1', 'Flow2', 'FlowC', 
                           'Pressure(Pa)', 'Temperature(°C)', 'BB(%)', 'ContTemp', 'SupplyTemp', 
                           'Status', 'ContStatus', 'DetectStatus', 'LedStatus', 'ValveStatus', 'LedTemp', 
                           'BC11', 'BC12', 'BC1', 'BC21', 'BC22', 'BC2', 'BC31', 'BC32', 'BC3', 'BC41', 
                           'BC42', 'BC4', 'BC51', 'BC52', 'BC5', 'BC61', 'BC62', 'BC6', 'BC71', 'BC72', 
                           'BC7', 'K1', 'K2', 'K3', 'K4', 'K5', 'K6', 'K7', 'TapeAdvCount', 
                           'X1', 'X2', 'X3', 'Time(UTC)', 'Latitude', 'NS', 'Longitude', 'EW', 'FixQuality', 
                           'NoSatellites', 'HDOP', 'Altitude', 'M1', 'H_WGS84', 'M2')) %>%
      map2(names(aeth_data), ~ mutate(.x, Trip = paste(str_split_fixed(.y, "_|\\.", n = 4)[1,1:2], collapse = "_"))) %>%
      # Remove any rows that have NA for latitude and longitude - solves Golden Trip2 problem and
      # can't use those rows anyway. 
      map(~ filter(.x, !(is.na(Latitude) & is.na(Longitude)))) %>%
      map(~ mutate(.x, Altitude = as.numeric(Altitude))) %>%
      map(~ select(.x, Date = `Date(yyyy/MM/dd)`, Time = `Time(hh:mm:ss)`,
                   UV_C = BC1, BC = BC6, Latitude, NS, Longitude, EW, Altitude, Trip)) %>%
      bind_rows() %>%
      # remove rows in which all columns are NA
      filter(rowSums(is.na(.)) != ncol(.)-1) %>%
      # convert readings to ug/m3 rather than ng/m3
      mutate(DateTime = paste(Date, Time),
             DateTime = as.POSIXct(DateTime, format = "%Y/%m/%d %H:%M:%S"),
             UV_C = ifelse(UV_C == 0, NA, UV_C),
             BC = ifelse(BC == 0, NA, BC),
             deltaC = UV_C - BC,
             UV_C = UV_C/1000,
             BC = BC/1000,
             deltaC = deltaC/1000,
             Latitude = ifelse(!is.na(NS) & NS == "S", Latitude*-1, Latitude),
             Lat_degs = as.numeric(substr(Latitude, 1, 2)),
             Lat_mins = as.numeric(substring(Latitude, 3, 4))/60,
             Lat_secs = (as.numeric(substring(Latitude, 5, 12))*60)/3600,
             Latitude = Lat_degs + Lat_mins + Lat_secs,
             Longitude = ifelse(!is.na(EW) & EW == "W", Longitude*-1, Longitude),
             Long_degs = as.numeric(substr(Longitude, 2, 4)),
             Long_mins = as.numeric(substring(Longitude, 5, 6))/60,
             Long_secs = (as.numeric(substring(Longitude, 7, 14))*60)/3600,
             Longitude = (Long_degs + Long_mins + Long_secs)*-1) %>%
      select(DateTime, Latitude, Longitude, Altitude, UV_C, BC, deltaC, Trip)
    
  })
  
  in_neph_data <- reactive({
    inFile <- input$NEPH_data
    
    req(inFile)
    validate(
      need(all(unlist(lapply(inFile[["name"]],
                             function(x) grepl(paste0("Trip", "[0-9][0-9]*", "_NEPH_", "20[0-9][0-9][0-9][0-9][0-9][0-9]"), x)))) != F,
           "Check that Nephelometer file names are formatted correctly")
    )
    
    neph_data <- map(1:nrow(inFile), ~ read_delim(file = inFile[[.x, "datapath"]],
                                                  delim = ",",
                                                  col_names = F,
                                                  col_types = cols(.default = "n", X1 = "c")))
    names(neph_data) <- inFile[["name"]]
    
    neph_data <- neph_data %>%
      map2(names(neph_data), ~ mutate(.x, Trip = paste(str_split_fixed(.y, "_|\\.", n = 4)[1,1:2], collapse = "_"))) %>%
      bind_rows() %>%
      rename(DateTime = X1, Bscat = X2, SampleT = X3, EnclosureT = X4,
             RH = X5, Pressure = X6) %>%
      mutate(DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"),
             Bscat = as.numeric(ifelse(Bscat == 0, NA, Bscat))) %>%
      select(DateTime, Bscat, SampleT, EnclosureT, RH, Pressure, Trip)
    
  })
  
# this reactive conductor cleans and combines all of the data for all trips together into dataframe
  
  all_trip_data <- reactive({
    
    trip_df <- list()
    
    for(i in 1:nrow(in_trip_list())){
      trip <- paste0("Trip", i)
      current_info <- in_trip_list()[i,]
      
      # Extract Trip Times and convert to POSIXct, adjusting date if time is after midnight
      start.time <- paste0(current_info$Date, ' ', current_info$Start)
      start.time <- as.POSIXct(start.time, format = "%Y-%m-%d %H:%M")
      end.time <- paste0(current_info$Date, ' ', current_info$End)
      end.time <- as.POSIXct(end.time, format = "%Y-%m-%d %H:%M")
      if(hour(end.time) < 6){
        end.time <- end.time + 86400
      }
      
      # Create expected length time sequence
      current_trip_df <- data.frame(seq(start.time, end.time, 1))
      colnames(current_trip_df) <- "DateTime"
      
      # Add column with TripCode
      current_trip_df$Trip <- trip
      
      # Merge the various datasets for this trip
      current_aeth <- filter(in_aeth_data(), Trip == paste0(trip, "_AE33")) %>%
        select(-Trip)
      current_trip_df <- merge(current_trip_df, current_aeth, by = "DateTime", all.x = T)
      
      if(input$include_NEPH){
        current_neph <- filter(in_neph_data(), Trip == paste0(trip, "_NEPH")) %>%
                                 select(-Trip)
        current_trip_df <- merge(current_trip_df, current_neph, by = "DateTime", all.x = T)
      }
      
      trip_df[[i]] <- current_trip_df
    }
    
    if(input$include_NEPH){
      trip_df <- bind_rows(trip_df) %>%
        DF.Replace.DC.Negatives(Max.Distance = 30) %>%
        DF.Replace.Bscat.Negatives(Max.Distance = 30) %>%
        select(DateTime, Trip, Latitude, Longitude, Bscat, deltaC, BS.CP, DC.CP) %>%
        mutate(BS.log = log(BS.CP),
               BS.log = ifelse(!is.finite(BS.log), NA, BS.log),
               DC.log = log(DC.CP), 
               DC.log = ifelse(!is.finite(DC.log), NA, DC.log)) %>%
        group_by(Trip) %>%
        mutate(Z.BS.log = as.numeric(scale(BS.log, center = T, scale = T)),
               Z.DC.log = as.numeric(scale(DC.log, center = T, scale = T)))
    } else{
      trip_df <- bind_rows(trip_df) %>%
        DF.Replace.DC.Negatives(Max.Distance = 30) %>%
        select(DateTime, Trip, Latitude, Longitude, deltaC, DC.CP) %>%
        mutate(DC.log = log(DC.CP), 
               DC.log = ifelse(!is.finite(DC.log), NA, DC.log)) %>%
        group_by(Trip) %>%
        mutate(Z.DC.log = as.numeric(scale(DC.log, center = T, scale = T)))
    }
    
    # Remove entries with min/max lat/long if specified
    if(input$min_lat){
      trip_df <- trip_df %>%
        filter(Latitude >= input$min_lat_num)
    }
    if(input$max_lat){
      trip_df <- trip_df %>%
        filter(Latitude <= input$max_lat_num)
    }
    if(input$min_long){
      trip_df <- trip_df %>%
        filter(Longitude >= input$min_long_num)
    }
    if(input$max_long){
      trip_df <- trip_df %>%
        filter(Latitude <= input$max_long_num)
    }
    
    trip_df
    
  })
  
  
# Reactive function to check fixed site latitude falls within the data
  in_fs_lat <- reactive({
    inFile <- input$fixed_site_lat
    req(inFile)

    validate(
      need(between(inFile, min(in_aeth_data()[,'Latitude'], na.rm = T), max(in_aeth_data()[,'Latitude'], na.rm = T)),
           "This latitude value is either outside the limits of the entered GPS data or in the incorrect format - please check value and formatting.")
    )
    
    fixed_site_lat <- inFile

  })

# Reactive function to check fixed site longitude falls within the data
  in_fs_long <- reactive({
    inFile <- input$fixed_site_long
    req(inFile)
    
    validate(
      need(between(inFile, min(in_aeth_data()[,'Longitude'], na.rm = T), max(in_aeth_data()[,'Longitude'], na.rm = T)),
           "This longitude value is either outside the limits of the entered GPS data or in the incorrect format - please check value and formatting.")
    )
    
    fixed_site_long <- inFile
    
  })
