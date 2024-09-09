library(httr)
library(jsonlite)

reverse_geocode <- function(lat, lon) {
  url <- paste0("https://nominatim.openstreetmap.org/reverse?lat=", lat, "&lon=", lon, "&format=json")
  response <- GET(url)
  content <- content(response, "text")
  json <- fromJSON(content)

  if (!is.null(json$address)) {
    address <- paste(json$address$road, json$address$suburb, json$address$city, sep = ", ")
    return(address)
  } else {
    return("Address not found")
  }
}

process_geospatial_data <- function(folder, input_file) {
  
  faasr_get_file(remote_folder=folder, remote_file=input_file, local_file="geospatial_data.csv")
  
  nypd_data <- read.csv("geospatial_data.csv", header = TRUE, sep = ",")
  
  geospatial_data <- nypd_data[, c('ARREST_KEY', 'ARREST_BORO', 'ARREST_PRECINCT', 'X_COORD_CD', 'Y_COORD_CD', 'Latitude', 'Longitude')]
  
  boro_map <- c(B = "Bronx", S = "Staten Island", K = "Brooklyn", M = "Manhattan", Q = "Queens")
  geospatial_data$ARREST_BORO <- boro_map[geospatial_data$ARREST_BORO]
  
  total_arrests <- nrow(geospatial_data)
  
  precinct_summary <- aggregate(ARREST_KEY ~ ARREST_PRECINCT, data = geospatial_data, FUN = length)
  most_common_precinct <- precinct_summary[which.max(precinct_summary$ARREST_KEY), "ARREST_PRECINCT"]
  least_common_precinct <- precinct_summary[which.min(precinct_summary$ARREST_KEY), "ARREST_PRECINCT"]
  
  boro_summary <- aggregate(ARREST_KEY ~ ARREST_BORO, data = geospatial_data, FUN = length)
  boro_summary$Percentage <- (boro_summary$ARREST_KEY / total_arrests) * 100
  
  most_common_borough <- boro_summary[which.max(boro_summary$ARREST_KEY), "ARREST_BORO"]
  least_common_borough <- boro_summary[which.min(boro_summary$ARREST_KEY), "ARREST_BORO"]
  
  lat_lon_summary <- aggregate(ARREST_KEY ~ Latitude + Longitude, data = geospatial_data, FUN = length)
  most_common_latlon <- lat_lon_summary[which.max(lat_lon_summary$ARREST_KEY), ]
  least_common_latlon <- lat_lon_summary[which.min(lat_lon_summary$ARREST_KEY), ]
  
  most_common_latlon_address <- reverse_geocode(most_common_latlon$Latitude, most_common_latlon$Longitude)
  least_common_latlon_address <- reverse_geocode(least_common_latlon$Latitude, least_common_latlon$Longitude)
  
  insights_row <- data.frame(
    Most_Common_Precinct = most_common_precinct,
    Least_Common_Precinct = least_common_precinct,
    Most_Common_Borough = most_common_borough,
    Least_Common_Borough = least_common_borough,
    Most_Common_LatLon = paste(most_common_latlon$Latitude, most_common_latlon$Longitude, sep = ", "),
    Most_Common_LatLon_Address = most_common_latlon_address,
    Least_Common_LatLon = paste(least_common_latlon$Latitude, least_common_latlon$Longitude, sep = ", "),
    Least_Common_LatLon_Address = least_common_latlon_address
  )
  
  write.csv(insights_row, file="geospatial_insights.csv", row.names=FALSE)
  
  write.csv(geospatial_data, file="processed_geospatial_data.csv", row.names=FALSE)
  
  faasr_put_file(local_file="geospatial_insights.csv", remote_folder="geospatial", remote_file="geospatial_insights.csv")
  faasr_put_file(local_file="processed_geospatial_data.csv", remote_folder="geospatial", remote_file="processed_geospatial_data.csv")
  
  log_msg <- paste0('Geospatial data processing complete; output files written to ', folder)
  faasr_log(log_msg)
}
