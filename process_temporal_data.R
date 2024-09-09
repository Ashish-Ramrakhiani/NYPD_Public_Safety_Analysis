process_temporal_data <- function(folder, input_file) {
  
  faasr_get_file(remote_folder=folder, remote_file=input_file, local_file="temporal_data.csv")
  
  nypd_data <- read.csv("temporal_data.csv", header = TRUE, sep = ",")
  
  nypd_data$ARREST_DATE <- as.Date(nypd_data$ARREST_DATE, format="%m/%d/%Y")
  
  nypd_data$Day <- weekdays(nypd_data$ARREST_DATE)
  
  nypd_data$Is_Weekend <- ifelse(nypd_data$Day %in% c("Saturday", "Sunday"), "Weekend", "Weekday")
  
  nypd_data$Month <- format(nypd_data$ARREST_DATE, "%B")
  
  temporal_data <- nypd_data[, c('ARREST_KEY', 'ARREST_DATE', 'Day', 'Is_Weekend', 'Month', 'PD_CD', 'PD_DESC', 'KY_CD', 'OFNS_DESC', 'LAW_CAT_CD')]
  
  total_crimes <- nrow(temporal_data)
  
  percentage_weekday <- sum(nypd_data$Is_Weekend == "Weekday") / total_crimes * 100
  percentage_weekend <- sum(nypd_data$Is_Weekend == "Weekend") / total_crimes * 100
  
  felony_count <- sum(nypd_data$LAW_CAT_CD == "F")
  misdemeanor_count <- sum(nypd_data$LAW_CAT_CD == "M")
  violation_count <- sum(nypd_data$LAW_CAT_CD == "V")
  
  percentage_felony <- felony_count / total_crimes * 100
  percentage_misdemeanor <- misdemeanor_count / total_crimes * 100
  percentage_violation <- violation_count / total_crimes * 100
  
  day_summary <- aggregate(ARREST_KEY ~ Day, data = nypd_data, FUN = length)
  most_crimes_day <- day_summary[which.max(day_summary$ARREST_KEY), "Day"]
  least_crimes_day <- day_summary[which.min(day_summary$ARREST_KEY), "Day"]
  
  month_summary <- aggregate(ARREST_KEY ~ Month, data = nypd_data, FUN = length)
  most_crimes_month <- month_summary[which.max(month_summary$ARREST_KEY), "Month"]
  least_crimes_month <- month_summary[which.min(month_summary$ARREST_KEY), "Month"]
  
  insights_df <- data.frame(
    Percentage_Weekday = percentage_weekday,
    Percentage_Weekend = percentage_weekend,
    Percentage_Felony = percentage_felony,
    Percentage_Misdemeanor = percentage_misdemeanor,
    Percentage_Violation = percentage_violation,
    Most_Crimes_Day = most_crimes_day,
    Least_Crimes_Day = least_crimes_day,
    Most_Crimes_Month = most_crimes_month,
    Least_Crimes_Month = least_crimes_month
  )
  
  write.csv(insights_df, file="temporal_insights.csv", row.names=FALSE)
  
  write.csv(temporal_data, file="processed_temporal_data.csv", row.names=FALSE)
  
  faasr_put_file(local_file="temporal_insights.csv", remote_folder="temporal", remote_file="temporal_insights.csv")
  faasr_put_file(local_file="processed_temporal_data.csv", remote_folder="temporal", remote_file="processed_temporal_data.csv")
  
  log_msg <- paste0('Temporal data preprocessing complete; output files written to ', folder)
  faasr_log(log_msg)
}
