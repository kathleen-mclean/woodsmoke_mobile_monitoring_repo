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
    
    variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
    
    # create empty raster stack and loop through individual trips - calculating the average pattern for each trip and storing it as a layer
    
    s <- stack()
    for (t in unique(trip_data$Trip)){
      
      shp <- SpatialPointsDataFrame(coords = data.frame(trip_data$Longitude[which(trip_data$Trip == t)], 
                                                        trip_data$Latitude[which(trip_data$Trip == t)]),
                                  data = trip_data[which(trip_data$Trip == t),],
                                  proj4string = CRS("+proj=longlat +datum=WGS84"))
      shp <- spTransform(shp, CRS("+init=epsg:3005")) 
      
      
      # Calculate rasters for that trip of counts and means per cell
      r.count <- rasterize(x = shp, y = create_raster(), field = shp@data[,variable],
                           fun = 'count', na.rm = T)
      r.mean <- rasterize(x = shp, y = create_raster(), field = shp@data[,variable],
                          fun = mean, na.rm = T)
      
      # Set the cells where there are less than 1 record to NA
      r.mean[which(r.count@data@values < 1)] <- NA
      r.count[which(r.count@data@values < 1)] <- NA
      
      # multiply the 2 rasters to create a mean*count layer
      r.MxC <- r.mean * r.count
      
      # Use focal function to perform focal smoothing on the count and mean*count layers
      #   Weighting is with an equally weighted 3x3 grid
      r.count.SMOOTHED <- focal(r.count, w=matrix(1,3,3), fun=sum, na.rm = T)
      r.MxC.SMOOTHED <- focal(r.MxC, w=matrix(1,3,3), fun=sum, na.rm = T)
      
      # Divide the smoothed mean*count by the smoothed count layer to effectively create a 
      #   mean layer focally smoothed using a 3x3 grid weighted by the cell counts
      r.mean.SMOOTHED <- r.MxC.SMOOTHED / r.count.SMOOTHED
      
      # add trip name to layer
      r.mean.SMOOTHED@data@names <- t
      
      # Add trip layer to the raster stack  
      s <- stack(s, r.mean.SMOOTHED)
    }

    # Calculate average pattern across all trips in the stack
    r.avg.pattern <- mean(s, na.rm = T)
    
    # Count the number of non NA values in each cell in the raster stack if there 
    # is more than one trip
    if(length(unique(trip_data$Trip)) > 1){
      
      rNA <- sum(!is.na(s))
      
      min.trips <- ceiling(length(unique(trip_data$Trip))*(3/4))
      
      # Set the values of cells in the average pattern layer to NA if the number of non NA values 
      #   in that cell were less than the total number of trips
      r.avg.pattern[which(rNA@data@values < min.trips)] <- NA
      
    }
    
    r.avg.pattern
  })
  
  trip_polygons <- reactive({
    
    # Convert to polygons 
    rtp <- rasterToPolygons(trip_raster())
    rtp@data$id <- 1:nrow(rtp@data)   # add id column for join data after the fortify
    rtp <- spTransform(rtp, CRS("+proj=longlat +datum=WGS84"))
    
    rtp$Z_binned <- ifelse(rtp$layer < -1.5, 1,
                           ifelse(rtp$layer >= -1.5 & rtp$layer < -1, 2,
                                  ifelse(rtp$layer >= -1 & rtp$layer < -0.5, 3, 
                                         ifelse(rtp$layer >= -0.5 & rtp$layer < 0, 4,
                                                ifelse(rtp$layer >= 0 & rtp$layer < 0.5, 5,
                                                       ifelse(rtp$layer >= 0.5 & rtp$layer < 1, 6,
                                                              ifelse(rtp$layer >= 1 & rtp$layer < 1.5, 7,
                                                                     ifelse(rtp$layer >= 1.5, 8, NA))))))))
    rtp$Z_binned <- factor(rtp$Z_binned, levels = c(1:8))
    
    binned_result <- rtp
    
    binned_result
  })
  
  trip_polygons_high <- reactive({
    
    high <- trip_polygons()[trip_polygons()$Z_binned %in% c(7, 8),]
    
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
  
  # Function for a custom leaflet legend, from:
  # https://stackoverflow.com/questions/52812238/custom-legend-with-r-leaflet-circles-and-squares-in-same-plot-legends
  
  addLegendCustom <- function(map, title, colors, labels, sizes, shapes, borders, opacity = 0.5){
    
    make_shapes <- function(colors, sizes, borders, shapes) {
      shapes <- gsub("circle", "50%", shapes)
      shapes <- gsub("square", "0%", shapes)
      paste0(colors, "; width:", sizes, "px; height:", sizes, "px; border:3px solid ", borders, "; border-radius:", shapes)
    }
    make_labels <- function(sizes, labels) {
      paste0("<div style='display: inline-block;height: ", 
             sizes, "px;margin-top: 4px;line-height: ", 
             sizes, "px;'>", labels, "</div>")
    }
    
    legend_colors <- make_shapes(colors, sizes, borders, shapes)
    legend_labels <- make_labels(sizes, labels)
    
    return(addLegend(map, title = title, colors = legend_colors, labels = legend_labels, opacity = opacity))
  }
  
  leaflet_trip_map <- reactive({
    
    YlOrBr <- brewer.pal(n = 9, "YlOrBr")[2:9]
    variable <- ifelse(input$include_NEPH & !is.null(input$varchoice), input$varchoice, "Z.DC.log")
    legend_label <- ifelse(variable == "Z.DC.log", "Delta C", "PM2.5 Estimate")
    variable_label <- ifelse(variable == "Z.DC.log", "Aethalometer Map", "Nephelometer Map")
    map_title <- paste0(unique(in_trip_list()$Community), " <br> Mobile Monitoring <br> ", variable_label)
    fixed_site_z_score <- ifelse(!is.null(fixed_site_value()), round(fixed_site_value(), 2), NA)
    convert_column <- ifelse(variable == "Z.DC.log", "Var", "PMP")
    var_values <- round(pm25_convert()[2:9, convert_column], 1)
    pm_legend_val <- round(pm25_convert()[1, convert_column], 1)
    
    polygons_df <- trip_polygons()
    polygons_high_df <- trip_polygons_high()
    mapbox_url <- paste0("https://api.mapbox.com/styles/v1/kathleenmclean/cjplrxlkx0fjy2sqnld49tuau/tiles/256/{z}/{x}/{y}@2x?access_token=pk.eyJ1Ijoia2F0aGxlZW5tY2xlYW4iLCJhIjoiY2pvcWdlYjR6MDRxNTN4cWlsaWZwN3VnMiJ9.OzAnOe2DjHW9dmBuExCU8Q")
    lat_center <- mean(c(min(all_trip_data()$Latitude, na.rm = T) - 0.01, 
                         max(all_trip_data()$Latitude, na.rm = T) + 0.01))
    long_center <- mean(c(min(all_trip_data()$Longitude, na.rm = T) - 0.01, 
                          max(all_trip_data()$Longitude, na.rm = T) + 0.01))
    pal <- colorFactor(palette = YlOrBr,
                       domain = polygons_df[["Z_binned"]])
    
    if(is.null(input$fixed_site_long) & is.null(input$fixed_site_lat)){
      fixed_site_long = 0
      fixed_site_lat = 0
    } else{
      fixed_site_long = input$fixed_site_long
      fixed_site_lat = input$fixed_site_lat
    }
    
    fixed_site_df <- data.frame(long = fixed_site_long,
                                lat = fixed_site_lat,
                                type = factor("Fixed Site Monitor"))
    pal2 <- colorFactor(palette = c("#000000"),
                        domain = fixed_site_df$type)
    
    tag.map.title <- tags$style(HTML("
    .leaflet-control.map-title { 
      transform: translate(-50%,20%);
      position: fixed !important;
      left: 42%;
      text-align: center;
      padding-left: 10px; 
      padding-right: 10px; 
      background: rgba(255,255,255,0.75);
      font-weight: bold;
      font-size: 18px;
    }
    "))
    
    title <- tags$div(
      tag.map.title, HTML(map_title)
    ) 
    
    map_attr <- "&copy; <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> &copy; <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a href='https://www.mapbox.com/map-feedback/' target='_blank'>Improve this map</a></strong>"
    
    final_map <- leaflet() %>%
      addTiles(urlTemplate = mapbox_url, attribution = map_attr) %>%
      setView(lng = long_center, lat = lat_center, zoom = as.integer(input$mapzoom)) %>%
      addPolygons(data = polygons_df,
                  fillColor = ~pal(Z_binned),
                  fillOpacity = input$mapopacity,
                  stroke = F) %>%
      addLegend("topright",
                colors = YlOrBr,
                title = paste0("Mean Z Score / ", legend_label, " (", "\u03BC", "g/m", "\u00B3", ")"),
                opacity = input$mapopacity,
                labels = c(paste0("  < -1.5          / < ", var_values[1]),
                           paste0("    -1.5 - -1.0   / ", var_values[1], " - ", var_values[2]),
                           paste0("    -1.0 - -0.5   / ", var_values[2], " - ", var_values[3]),
                           paste0("    -0.5 -  0.0   / ", var_values[3], " - ", var_values[4]),
                           paste0("     0.0 -  0.5   / ", var_values[4], " - ", var_values[5]),
                           paste0("     0.5 -  1.0   / ", var_values[5], " - ", var_values[6]),
                           paste0("     1.0 -  1.5   / ", var_values[6], " - ", var_values[7]),
                           paste0("     1.5 +        / ", var_values[7], " +"))) %>%
      # addPolygons(data = polygons_high_df,
      #             fillColor = ~pal(Z_binned),
      #             fillOpacity = 0.9,
      #             stroke = F) %>%
      addControl(title, position = "topleft", className="map-title") %>%
      addControl(ifelse(variable == "Z.DC.log", 
                        "This map shows the average spatial patterns captured by an Aethalometer \nwhich measures a signal specific to woodsmoke called 'Delta C'.",
                        "This map shows the average spatial patterns captured by a Nephelometer \nwhich measures an estimate of total PM2.5 levels."),
                 position = "bottomleft")

    if(fixed_site_long != 0 & fixed_site_lat != 0 &
       !is.na(fixed_site_long) & !is.na(fixed_site_lat)){
      final_map <- final_map %>%
        addCircleMarkers(data = fixed_site_df, radius = 7, 
                         opacity = 1, stroke = T, weight = 4,
                         color = ~pal2(type), fill = F) %>%
        addLegendCustom(colors = c("white"), 
                  title = "Monitoring Station",
                  sizes = c(20),
                  shapes = c("circle"),
                  borders = c("#000000"),
                  opacity = 1,
                  labels = paste0("Mean Z Score = ", fixed_site_z_score, " / ", 
                                  legend_label, " = ", pm_legend_val, " (", "\u03BC", "g/m", "\u00B3", ")"))
    }
    
    final_map
  })
  
  # This function gets the leaflet map for the downloaded file at the same 
  # center and zoom as whatever the user has chosen in the viewing pane
  
  user_leaflet_map <- reactive({
    
    leaflet_trip_map() %>%
      setView(lng = input$map_center$lng, 
              lat = input$map_center$lat,
              zoom = input$map_zoom)
  })