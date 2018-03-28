###################################################################################
# FUNCTION - Replace.Negatives.Closest.Positive(Vector, Max.Distance, Variable.Name)
#     - Replaces negative readings with the closest positive reading 
#           (if there is one within Max.Distance, otherwise sets to NA)
#
# Vector = Vector of interest
# Max.Distance = Maximum distance of closest positive value from negative value to 
#                   use the positive value. If no positive values within this distance,
#                     negative value replaced with NA.
# Variable.Name = name of variable for printing update
###################################################################################

Replace.Negatives.Closest.Positive <- function(Vector, Max.Distance, Variable.Name){
  
  # Extract the vector of interest and create an index of positive and negative values
  PN.Index <- Vector
  PN.Index[which(is.na(Vector) == T)] <- 'NA'
  PN.Index[which(Vector < 0)] <- 'N'
  PN.Index[which(Vector >= 0)] <- 'P'
  
  Replaced.Pos.Counter = 0
  Replaced.NA.Counter = 0
  
  # Loop through Vector - if the index at that position is negative, 
  # replace the negative value with the closest positive value
  for(i in 1:length(Vector)){
    if(PN.Index[i] == 'N'){
      Position.Diff <- abs(1:length(Vector) - i)              # create a position difference vector
      Position.Diff[which(PN.Index == 'NA')] <- NA        # set values where the original data was NA to NA
      Position.Diff[which(PN.Index == 'N')] <- NA             # set values where the original data was negative to NA
      Closest.Positive.Position <- which.min(Position.Diff)   # find the closest positive position
      Distance <- Position.Diff[Closest.Positive.Position]    # check the distance to that position
      
      # if the closest positive value is within the defined Max.Distance, 
      #       replace the data value with that closest positive value, otherwise replace with NA
      if(Distance <= Max.Distance){
        Vector[i] <- Vector[Closest.Positive.Position]
        Replaced.Pos.Counter <- Replaced.Pos.Counter + 1
      } else{
        Vector[i] <- NA
        Replaced.NA.Counter <- Replaced.NA.Counter + 1
      }
    }
  }
  
  return(Vector)
}

###################################################################################
# FUNCTION - DF.Replace.DC.Bscat.Negatives(DF, Max.Distance)
#     - Adds 2 columns each of DC and Bscat where negative readings have been replaced
#         with either the closest positive (within Max.Distance) or zeros
#
# DF = Dataframe
# Max.Distance = Maximum distance of closest positive value from negative value to 
#                   use the positive value. If no positive values within this distance,
#                     negative value replaced with NA.
###################################################################################

DF.Replace.DC.Negatives <- function(DF, Max.Distance){
  
  DC.CP <- NULL
  
  for(trip in unique(DF$Trip)){
    # subset the trip data
    Trip.Data <- DF[which(DF$Trip == trip),]
    # run the closest positives function for delta C and replace with zeros function
    Trip.DC.CP <- Replace.Negatives.Closest.Positive(Vector = Trip.Data$deltaC, Max.Distance = Max.Distance, Variable.Name = 'Delta C')
    
    DC.CP <- c(DC.CP, Trip.DC.CP)
    
    rm(trip, Trip.Data, Trip.DC.CP)
  }
  
  DF$DC.CP <- DC.CP
  
  return(DF)
  
}

DF.Replace.Bscat.Negatives <- function(DF, Max.Distance){
  
  BS.CP <- NULL
  
  for(trip in unique(DF$Trip)){
    # subset the trip data
    Trip.Data <- DF[which(DF$Trip == trip),]
  
    # run the closest positives function for BS and replace with zeros function
    Trip.BS.CP <- Replace.Negatives.Closest.Positive(Vector = Trip.Data$Bscat, Max.Distance = Max.Distance, Variable.Name = 'Bscat')
    
    BS.CP <- c(BS.CP, Trip.BS.CP)
    
    rm(trip, Trip.Data, Trip.BS.CP)
  }
  
  DF$BS.CP <- BS.CP
  
  return(DF)
  
}
