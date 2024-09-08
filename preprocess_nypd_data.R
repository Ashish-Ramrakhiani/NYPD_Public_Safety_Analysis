preprocess_nypd_data <- function(folder, input_file) {
  
  faasr_get_file(remote_folder=folder, remote_file=input_file, local_file="nypd_data.csv")
  nypd_data <- read.csv("nypd_data.csv", header = TRUE, sep = ",")
  
  # Handling missing values
  # Example: Replacing missing values in 'PERP_RACE' with 'Unknown'
  nypd_data$PERP_RACE[is.na(nypd_data$PERP_RACE)] <- 'Unknown'
  nypd_data$PERP_SEX[is.na(nypd_data$PERP_SEX)] <- 'Unknown'
  
  # For numeric columns, replace NA with the column mean
  #nypd_data$X_COORD_CD[is.na(nypd_data$X_COORD_CD)] <- mean(nypd_data$X_COORD_CD, na.rm = TRUE)
  #nypd_data$Y_COORD_CD[is.na(nypd_data$Y_COORD_CD)] <- mean(nypd_data$Y_COORD_CD, na.rm = TRUE)
  
  nypd_data$PD_CD[is.na(nypd_data$PD_CD) | nypd_data$PD_CD == ""] <- 0
  nypd_data$KY_CD[is.na(nypd_data$KY_CD) | nypd_data$KY_CD == ""] <- 0
  
  nypd_data$PD_DESC[is.na(nypd_data$PD_DESC) | nypd_data$PD_DESC == "(null)"] <- "Unknown"
  nypd_data$OFNS_DESC[is.na(nypd_data$OFNS_DESC) | nypd_data$OFNS_DESC == "(null)"] <- "Unknown"
  
  # Creating splits for 3 CSV files: Temporal, Demographic, and Geospatial
  
  # 1. Temporal Data: ARREST_KEY, ARREST_DATE, KY_CD, LAW_CAT_CD, PD_DESC, OFNS_DESC
  temporal_data <- nypd_data[, c('ARREST_KEY', 'ARREST_DATE', 'PD_CD','PD_DESC','KY_CD','OFNS_DESC','LAW_CAT_CD')]
  
  # 2. Demographic Data: ARREST_KEY, AGE_GROUP, PERP_SEX, PERP_RACE
  demographic_data <- nypd_data[, c('ARREST_KEY', 'AGE_GROUP', 'PERP_SEX', 'PERP_RACE')]
  
  # 3. Geospatial Data: ARREST_KEY, ARREST_BORO, ARREST_PRECINCT, X_COORD_CD, Y_COORD_CD, Latitude, Longitude
  geospatial_data <- nypd_data[, c('ARREST_KEY', 'ARREST_BORO', 'ARREST_PRECINCT', 'X_COORD_CD', 'Y_COORD_CD', 'Latitude', 'Longitude')]
  
  # Saving the three splits as CSV files
  write.csv(temporal_data, file="temporal_data.csv", row.names=FALSE)
  write.csv(demographic_data, file="demographic_data.csv", row.names=FALSE)
  write.csv(geospatial_data, file="geospatial_data.csv", row.names=FALSE)
  
  # Upload the split files back to the bucket
  faasr_put_file(local_file="temporal_data.csv", remote_folder=folder, remote_file="temporal_data.csv")
  faasr_put_file(local_file="demographic_data.csv", remote_folder=folder, remote_file="demographic_data.csv")
  faasr_put_file(local_file="geospatial_data.csv", remote_folder=folder, remote_file="geospatial_data.csv")
  
  # Log message to confirm completion
  log_msg <- paste0('Preprocessing and splitting complete; output files written to ', folder)
  faasr_log(log_msg)
}
