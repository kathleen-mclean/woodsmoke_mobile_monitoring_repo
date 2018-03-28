pm25_convert <- reactive({
  
  variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
  DF <- data.frame("Value" = c('Monitoring Station', -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 'Min', 'Max'),
                   "Zscore" = c(fixed_site_value(), 
                           -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 
                           min(trip_polygons()$layer), 
                           max(trip_polygons()$layer)),
                   stringsAsFactors = F)
  DF$LogVar <- NA
  DF$Var <- NA
  DF$PMP <- NA # pooled
  
  if(variable == "Z.DC.log"){
    
    # calculate mean and SD of the column used to create the Z scores
    M.DC.Log <- mean(all_trip_data()$DC.log, na.rm = T)
    SD.DC.Log <- sd(all_trip_data()$DC.log, na.rm = T)
    
    # Use the M and SD to reverse the Z score
    for(i in 1:nrow(DF)){
      DF$LogVar[i] <- M.DC.Log+(DF$Zscore[i]*SD.DC.Log)
    }
    
    # Take the exponent to convert Log-transformed data back into raw
    DF$Var <- exp(DF$LogVar)
  } else{
    
    # calculate mean and SD of the column used to create the Z scores
    M.BS.Log <- mean(all_trip_data()$BS.log, na.rm = T)
    SD.BS.Log <- sd(all_trip_data()$BS.log, na.rm = T)
    
    # Use the M and SD to reverse the Z score
    for(i in 1:nrow(DF)){
      DF$LogVar[i] <- M.BS.Log+(DF$Zscore[i]*SD.BS.Log)
    }
    
    # Take the exponent to convert Log-transformed data back into raw
    DF$Var <- exp(DF$LogVar)
    
    # Convert the Bscat values to PM estimates
    for(i in 1:nrow(DF)){
      # DF$PMC[i] <- (BS.Conv.Slope.C*DF$Bscat[i]) + BS.Conv.Int.C
      DF$PMP[i] <- (Neph.Conversions[[4]][[2]]*DF$Var[i]) + Neph.Conversions[[4]][[1]]
    }
  }
  
  DF
  
})