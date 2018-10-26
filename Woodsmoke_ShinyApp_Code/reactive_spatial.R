# These reactive conductors are related to the spatial analysis and mapping part of the app

  create_raster <- reactive({
    
    xtll <- data.frame(matrix(nrow = 2, ncol = 2), stringsAsFactors = F)
    xtll[1,1] <- c(min(all_trip_data()$Latitude, na.rm = T) - 0.02) # X min
    xtll[1,2] <- c(min(all_trip_data()$Longitude, na.rm = T) - 0.02)   # y min
    xtll[2,1] <- c(max(all_trip_data()$Latitude, na.rm = T) + 0.02) # x max
    xtll[2,2] <- c(max(all_trip_data()$Longitude, na.rm = T) + 0.02)   # y max
    names(xtll) <- c("Latitude", "Longitude")
    
    # Convert to SpatialPoints with world epsg:4326
    coordinates(xtll) <- ~ Longitude + Latitude
    proj4string(xtll) <- CRS("+init=epsg:4326")
    
    # convert CRS to EPSG:3005 (NAD83/BC Albers)
    xtll2 <- spTransform(xtll, CRS("+init=epsg:3005"))
    
    # Take the extent from the previous result and round to nearest 10m
    xtll2ext <- vapply(extent(xtll2), FUN = round, -1, FUN.VALUE = numeric(1))
    
    ext = extent(xtll2ext)
    
    # Determine ncol and nrow by counting the number of rows and columns to make each
    #    side of a 3x3 square = approx 100m
    num_columns <- round(length(ext[1]:ext[2])/(100/3), 0)
    num_rows <- round(length(ext[3]:ext[4])/(100/3), 0)
    
    # create raster with these extents, calculated # of columns and under same projection
    output.raster = raster(ext, ncol = num_columns, nrow = num_rows, crs="+init=epsg:3005")
    output.raster
  })
  
  trip_raster <- reactive({
    
    if(input$mapchoice == "night"){
      trips <- in_trip_list() %>% 
        filter(`Night/Day` == "Night") %>%
        select(Trip)
    } else {
      trips <- in_trip_list() %>%
        select(Trip)
    }
    
    trip_data <- all_trip_data() %>%
      filter(Trip %in% paste0("Trip", trips$Trip) & 
               !is.na(Latitude) & !is.na(Longitude))
    
    shp <- SpatialPointsDataFrame(coords = data.frame(trip_data$Longitude, trip_data$Latitude),
                                  data = trip_data,
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))
    shp <- spTransform(shp, CRS("+init=epsg:3005"))

    variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")

    r.count <- rasterize(x = shp, y = create_raster(), field = shp@data[,variable],
                         fun = 'count', na.rm = T)
    r.mean <- rasterize(x = shp, y = create_raster(), field = shp@data[,variable],
                        fun = mean, na.rm = T)
    r.n.trips <- rasterize(x = shp, y = create_raster(), field = shp@data[,'Trip'],
                           fun = function(x, ...){ length(unique(na.omit(x)))}, na.rm = T)
    
    # Set the values of cells which were missed on more than 1 trip to NA
    r.mean[which(r.n.trips@data@values < max(r.n.trips@data@values, na.rm = T) - 1)] <- NA
    r.count[which(r.n.trips@data@values < max(r.n.trips@data@values, na.rm = T) - 1)] <- NA

    # multiply the 2 rasters to create a mean*count layer
    r.MxC <- r.mean * r.count

    # Use focal function to perform focal smoothing on the count and mean*count layers
    #   Weighting is with an equally weighted 3x3 grid
    r.count.SMOOTHED <- focal(r.count, w=matrix(1,3,3), fun=sum, na.rm = T)
    r.MxC.SMOOTHED <- focal(r.MxC, w=matrix(1,3,3), fun=sum, na.rm = T)

    # Divide the smoothed mean*count by the smoothed count layer to effectively create a
    #   mean layer focally smoothed using a 3x3 grid weighted by the cell counts
    r.mean.SMOOTHED <- r.MxC.SMOOTHED / r.count.SMOOTHED

    r.mean.SMOOTHED
  })
  
  trip_polygons <- reactive({
    
    # Convert to polygons 
    rtp <- rasterToPolygons(trip_raster())
    rtp@data$id <- 1:nrow(rtp@data)   # add id column for join data after the fortify
    rtp <- spTransform(rtp, CRS("+proj=longlat +datum=WGS84"))
    
    # convert to normal dataframe and merge the data to it
    rtpFort <- fortify(rtp, data = rtp@data)
    rtpFortMer <- merge(rtpFort, rtp@data, by.x = 'id', by.y = 'id')  # join data
    
    binned_result <- rtpFortMer %>%
      mutate(Z_binned = case_when(
        layer < -1.5 ~ 1,
        layer >= -1.5 & layer < -1 ~ 2,
        layer >= -1 & layer < -0.5 ~ 3,
        layer >= -0.5 & layer < 0 ~ 4,
        layer >= 0 & layer < 0.5 ~ 5,
        layer >= 0.5 & layer < 1 ~ 6,
        layer >= 1 & layer < 1.5 ~ 7,
        layer >= 1.5 ~ 8),
        Z_binned = factor(Z_binned, levels = c(1:8)))
    
    binned_result
  })
  
  trip_polygons_high <- reactive({
    
    high <- filter(trip_polygons(), Z_binned %in% c(7, 8))
    
    high
  })
  
  fixed_site_value <- reactive({
    
    if(is.null(input$fixed_site_long) & is.null(input$fixed_site_lat)){
      fixed_site_long = 0
      fixed_site_lat = 0
    } else{
      fixed_site_long = input$fixed_site_long
      fixed_site_lat = input$fixed_site_lat
    }
    
    if(fixed_site_long != 0 & fixed_site_lat != 0 &
       !is.na(fixed_site_long) & !is.na(fixed_site_lat)){
    
      # Convert fixed site coordinates to match raster layer
      fs.location <- SpatialPoints(cbind(fixed_site_long, fixed_site_lat), proj4string = CRS("+proj=longlat +datum=WGS84"))
      fs.location <- spTransform(fs.location, CRS("+init=epsg:3005"))
      
      # Extract the value of the cell that the fixed site coordinates fall within 
      result <- extract(trip_raster(), fs.location)
      
      result
    }
    
  })
  
  base_map <- reactive({
    
    get_map(location = c(min(all_trip_data()$Longitude, na.rm = T) - 0.01,
                         min(all_trip_data()$Latitude, na.rm = T) - 0.01,
                         max(all_trip_data()$Longitude, na.rm = T) + 0.01,
                         max(all_trip_data()$Latitude, na.rm = T) + 0.01),
            zoom = as.integer(input$mapzoom),
            source = "stamen",
            maptype = "terrain",
            scale = "auto",
            color = "color")
    
    # Turned off because google switched to needing an API key with credit card information for billing
    # get_googlemap(center = c(lon = mean(c(min(all_trip_data()$Longitude, na.rm = T) - 0.01, 
    #                                       max(all_trip_data()$Longitude, na.rm = T) + 0.01)),
    #                          lat = mean(c(min(all_trip_data()$Latitude, na.rm = T) - 0.01, 
    #                                       max(all_trip_data()$Latitude, na.rm = T) + 0.01))),
    #               zoom = as.integer(input$mapzoom),
    #               maptype = "roadmap",
    #               color = "color",
    #               style = "feature:road|element:labels|visibility:off&style=feature:administrative|element:labels|visibility:off&style=feature:poi|element:labels|visibility:off")
    
  })
  
  trip_map <- reactive({
    
    YlOrBr <- brewer.pal(n = 9, "YlOrBr")[2:9]
    variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
    legend_label <- ifelse(variable == "Z.DC.log", "Delta C", "PM2.5 Estimate")
    variable_label <- ifelse(variable == "Z.DC.log", "Aethalometer Map", "Nephelometer Map")
    map_title <- in_trip_list()$Community
    map_subtitle <- paste0("Mobile Monitoring \n", variable_label)
    fixed_site_z_score <- ifelse(!is.null(fixed_site_value()), round(fixed_site_value(), 2), NA)
    convert_column <- ifelse(variable == "Z.DC.log", "Var", "PMP")
    var_values <- round(pm25_convert()[2:9, convert_column], 1)
    pm_legend_val <- round(pm25_convert()[1, convert_column], 1)
    
    final_map <- ggmap(base_map()) +
      geom_polygon(data = trip_polygons(), 
                   aes(x = long, y = lat, group = group, fill = Z_binned), 
                   alpha = 0.9, 
                   size = 0) + ## size = 0 to remove the polygon outlines
      scale_fill_manual(values = YlOrBr,
                        guide = guide_legend(title = paste0("Mean Z Score / ", legend_label, " (", "\u03BC", "g/m", "\u00B3", ")")),
                        labels = c(paste0("  < -1.5          / < ", var_values[1]),
                                   paste0("    -1.5 - -1.0   / ", var_values[1], " - ", var_values[2]),
                                   paste0("    -1.0 - -0.5   / ", var_values[2], " - ", var_values[3]),
                                   paste0("    -0.5 -  0.0   / ", var_values[3], " - ", var_values[4]), 
                                   paste0("     0.0 -  0.5   / ", var_values[4], " - ", var_values[5]), 
                                   paste0("     0.5 -  1.0   / ", var_values[5], " - ", var_values[6]), 
                                   paste0("     1.0 -  1.5   / ", var_values[6], " - ", var_values[7]),
                                   paste0("     1.5 +        / ", var_values[7], " +")),
                        drop = F) +
      geom_polygon(data = trip_polygons_high(), 
                   aes(x = long, y = lat, group = group, fill = Z_binned), 
                   alpha = 1, 
                   size = 0, 
                   show.legend = F) +
      theme(axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 20, hjust = 0.5),
            legend.position = "right",
            legend.box = "vertical",
            legend.key.height = unit(40, units = "pt")) +
      ggtitle(map_title, subtitle = map_subtitle)
    
    if(is.null(input$fixed_site_long) & is.null(input$fixed_site_lat)){
      fixed_site_long = 0
      fixed_site_lat = 0
    } else{
      fixed_site_long = input$fixed_site_long
      fixed_site_lat = input$fixed_site_lat
    }
    
    if(fixed_site_long != 0 & fixed_site_lat != 0 &
       !is.na(fixed_site_long) & !is.na(fixed_site_lat)){
      final_map <- final_map +
        geom_point(data = data.frame(long = fixed_site_long,
                                     lat = fixed_site_lat,
                                     type = "Fixed site monitor"),
                   aes(x = long, y = lat, shape = type),
                   size = 4, fill = "black") +
        scale_shape_manual(name = "",
                           labels = paste0("Monitoring Station\n Mean Z Score = ",
                                           fixed_site_z_score, "\n", legend_label,
                                           " = ", pm_legend_val, " (", "\u03BC", "g/m", "\u00B3", ")"),
                           values = c(5),
                           guide = guide_legend(order = 0))
    }
    
    # final_map_g <- ggplotGrob(final_map)
    # str(final_map_g)
    
    final_map
  })