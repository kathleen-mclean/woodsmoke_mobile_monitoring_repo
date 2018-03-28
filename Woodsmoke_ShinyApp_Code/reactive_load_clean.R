# These reactive conductors load and clean the data input to the fileInputs on the UI

  in_trip_list <- reactive({
    inFile <- input$trip_list
    
    req(inFile)
    
    trip_data <- read_csv(file = inFile$datapath)
    
  })

  in_gps_data <- reactive({
    inFile <- input$GPS_data
    
    req(inFile)
    
    gps_data <- map(1:nrow(inFile), ~ read_csv(file = inFile[[.x, "datapath"]]))
    names(gps_data) <- inFile[["name"]]
    
    gps_data <- gps_data %>%
      map2(names(gps_data), ~ mutate(.x, Trip = str_split_fixed(.y, "\\.", n = 2)[1,1])) %>%
      map(~ select(.x, Date, Time, Latitude, Longitude, Speed_kph = `Speed(km/hour)`, Trip)) %>%
      bind_rows() %>%
      mutate(DateTime = paste(Date, Time),
             DateTime = as.POSIXct(DateTime, format = "%Y-%m-%d %H:%M:%S"),
             Lat_degs = as.numeric(substr(Latitude, 1, 2)),
             Lat_mins = as.numeric(substring(Latitude, 3, 4))/60,
             Lat_secs = (as.numeric(substring(Latitude, 5, 12))*60)/3600,
             Latitude = Lat_degs + Lat_mins + Lat_secs,
             Long_degs = as.numeric(substr(Longitude, 2, 4)),
             Long_mins = as.numeric(substring(Longitude, 5, 6))/60,
             Long_secs = (as.numeric(substring(Longitude, 7, 14))*60)/3600,
             Longitude = (Long_degs + Long_mins + Long_secs)*-1) %>%
      select(DateTime, Latitude, Longitude, Speed_kph, Trip)
    
  })


  in_aeth_data <- reactive({
    inFile <- input$AETH_data
    
    req(inFile)
    
    aeth_data <- map(1:nrow(inFile), ~ read.table(file = inFile[[.x, "datapath"]],
                                                  header = T, skip = 5, 
                                                  stringsAsFactors = F))
    names(aeth_data) <- inFile[["name"]]
    
    aeth_data <- aeth_data %>%
      map2(names(aeth_data), ~ mutate(.x, Trip = str_split_fixed(.y, "\\.", n = 2)[1,1])) %>%
      map(~ select(.x, Date = Date.yyyy.MM.dd.., Time = Time.hh.mm.ss.., 
                   UV_C = BC1., BC = BC6., Trip)) %>%
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
             deltaC = deltaC/1000) %>%
      select(DateTime, UV_C, BC, deltaC, Trip)
    
  })
  
  in_neph_data <- reactive({
    inFile <- input$NEPH_data
    
    req(inFile)
    
    neph_data <- map(1:nrow(inFile), ~ read_delim(file = inFile[[.x, "datapath"]],
                                                  delim = ",",
                                                  col_names = F,
                                                  col_types = cols(.default = "n", X1 = "c")))
    names(neph_data) <- inFile[["name"]]
    
    neph_data <- neph_data %>%
      map2(names(neph_data), ~ mutate(.x, Trip = str_split_fixed(.y, "\\.", n = 2)[1,1])) %>%
      bind_rows() %>%
      rename(DateTime = X1, Bscat = X2, SampleT = X3, EnclosureT = X4,
             RH = X5, Pressure = X6) %>%
      mutate(DateTime = as.POSIXct(DateTime, format = "%d/%m/%Y %H:%M:%S"),
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
      start.time <- paste0('20', current_info$Date, ' ', current_info$Start)
      start.time <- as.POSIXct(start.time, format = "%Y-%m-%d %H:%M")
      end.time <- paste0('20', current_info$Date, ' ', current_info$End)
      end.time <- as.POSIXct(end.time, format = "%Y-%m-%d %H:%M")
      if(hour(end.time) < 6){
        end.time <- end.time + 86400
      }
      
      # Create expected length time sequence
      current_trip_df <- data.frame(seq(start.time, end.time, 1))
      colnames(current_trip_df) <- "DateTime"
      
      # # Classify expected times as co-location or trip
      # current_trip_df$TorC <- NA
      # if(is.na(on.time) == F){
      #   current_trip_df$TorC[which(current_trip_df$DateTime >= on.time & current_trip_df$DateTime < start.time)] <- 'C'
      # }
      # 
      # current_trip_df$TorC[which(current_trip_df$DateTime >= start.time & current_trip_df$DateTime <= end.time)] <- 'T'
      # 
      # if(is.na(off.time) == F){
      #   current_trip_df$TorC[which(current_trip_df$DateTime > end.time & current_trip_df$DateTime < off.time)] <- 'C'
      # }
      
      # Add column with TripCode
      current_trip_df$Trip <- trip
      
      # Merge the various datasets for this trip
      current_gps <- filter(in_gps_data(), Trip == paste0(trip, "_GPS")) %>%
        select(-Trip)
      current_trip_df <- merge(current_trip_df, current_gps, by = "DateTime", all.x = T)
      
      current_aeth <- filter(in_aeth_data(), Trip == paste0(trip, "_AE33")) %>%
        select(-Trip)
      current_trip_df <- merge(current_trip_df, current_aeth, by = "DateTime", all.x = T)
      
      if(input$include_NEPH){
        current_neph <- filter(in_neph_data(), Trip == paste0(trip, "_Aurora")) %>%
                                 select(-Trip)
        current_trip_df <- merge(current_trip_df, current_neph, by = "DateTime", all.x = T)
      }
      
      trip_df[[i]] <- current_trip_df
    }
    
    if(input$include_NEPH){
      trip_df <- bind_rows(trip_df) %>%
        DF.Replace.DC.Negatives(Max.Distance = 30) %>%
        DF.Replace.Bscat.Negatives(Max.Distance = 30) %>%
        # filter(TorC == "T") %>%
        select(DateTime, Trip, Latitude, Longitude, Speed_kph, Bscat, deltaC, BS.CP, DC.CP) %>%
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
        # filter(TorC == "T") %>%
        select(DateTime, Trip, Latitude, Longitude, Speed_kph, deltaC, DC.CP) %>%
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
  