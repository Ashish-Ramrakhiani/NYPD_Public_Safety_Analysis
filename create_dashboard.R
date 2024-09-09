create_dashboard <- function(folder_temporal, temporal_file, folder_demo,demographic_file, folder_geo,geospatial_file) {
  
  faasr_get_file(remote_folder=folder_temporal, remote_file=temporal_file, local_file="temporal_insights.csv")
  faasr_get_file(remote_folder=folder_demo, remote_file=demographic_file, local_file="demographic_insights.csv")
  faasr_get_file(remote_folder=folder_geo, remote_file=geospatial_file, local_file="geospatial_insights.csv")
  
  temporal_data <- read.csv("temporal_insights.csv", header = TRUE, sep = ",")
  demographic_data <- read.csv("demographic_insights.csv", header = TRUE, sep = ",")
  geospatial_data <- read.csv("geospatial_insights.csv", header = TRUE, sep = ",")
  
  temporal_insights <- temporal_data[, c("Percentage_Weekday", "Percentage_Weekend", "Percentage_Felony", 
                                          "Percentage_Misdemeanor", "Percentage_Violation", 
                                          "Most_Crimes_Day", "Least_Crimes_Day", "Most_Crimes_Month", 
                                          "Least_Crimes_Month")]
  
  demographic_insights <- demographic_data[, c("Most_Common_Age_Group", "Least_Common_Age_Group", 
                                                "Most_Common_Sex", "Most_Common_Race", 
                                                "Least_Common_Race", "Most_Common_Age_Race_Combination", 
                                                "Least_Common_Age_Race_Combination")]
  
  geospatial_insights <- geospatial_data[, c("Most_Common_Precinct", "Least_Common_Precinct", 
                                              "Most_Common_Borough", "Least_Common_Borough", 
                                              "Most_Common_LatLon", "Most_Common_LatLon_Address", 
                                              "Least_Common_LatLon", "Least_Common_LatLon_Address")]
  
  combined_insights <- data.frame(
    "Percentage of Arrests on Weekdays" = temporal_insights$Percentage_Weekday,
    "Percentage of Arrests on Weekends" = temporal_insights$Percentage_Weekend,
    "Percentage of Felony Arrests" = temporal_insights$Percentage_Felony,
    "Percentage of Misdemeanor Arrests" = temporal_insights$Percentage_Misdemeanor,
    "Percentage of Violation Arrests" = temporal_insights$Percentage_Violation,
    "Day with Most Crimes" = temporal_insights$Most_Crimes_Day,
    "Day with Least Crimes" = temporal_insights$Least_Crimes_Day,
    "Month with Most Crimes" = temporal_insights$Most_Crimes_Month,
    "Month with Least Crimes" = temporal_insights$Least_Crimes_Month,
    
    "Most Common Age Group Involved in Arrests" = demographic_insights$Most_Common_Age_Group,
    "Least Common Age Group Involved in Arrests" = demographic_insights$Least_Common_Age_Group,
    "Most Common Sex Involved in Arrests" = demographic_insights$Most_Common_Sex,
    "Most Common Race Involved in Arrests" = demographic_insights$Most_Common_Race,
    "Least Common Race Involved in Arrests" = demographic_insights$Least_Common_Race,
    "Most Common Age and Race Combination" = demographic_insights$Most_Common_Age_Race_Combination,
    "Least Common Age and Race Combination" = demographic_insights$Least_Common_Age_Race_Combination,
    
    "Precinct with Most Arrests" = geospatial_insights$Most_Common_Precinct,
    "Precinct with Least Arrests" = geospatial_insights$Least_Common_Precinct,
    "Borough with Most Arrests" = geospatial_insights$Most_Common_Borough,
    "Borough with Least Arrests" = geospatial_insights$Least_Common_Borough,
    "Most Common Arrest Location (Lat/Lon)" = geospatial_insights$Most_Common_LatLon,
    "Most Common Arrest Location (Address)" = geospatial_insights$Most_Common_LatLon_Address,
    "Least Common Arrest Location (Lat/Lon)" = geospatial_insights$Least_Common_LatLon,
    "Least Common Arrest Location (Address)" = geospatial_insights$Least_Common_LatLon_Address
  )
  
  write.csv(combined_insights, file="dashboard.csv", row.names=FALSE)
  
  faasr_put_file(local_file="dashboard.csv", remote_folder="dashboard", remote_file="dashboard.csv")
  
  log_msg <- paste0('Dashboard of insights created; output file written to dashboard')
  faasr_log(log_msg)
}
