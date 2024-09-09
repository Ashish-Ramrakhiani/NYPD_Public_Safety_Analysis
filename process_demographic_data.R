process_demographic_data <- function(folder, input_file) {
  
  faasr_get_file(remote_folder=folder, remote_file=input_file, local_file="demographic_data.csv")
  
  nypd_data <- read.csv("demographic_data.csv", header = TRUE, sep = ",")
  
  demographic_data <- nypd_data[, c('ARREST_KEY', 'AGE_GROUP', 'PERP_SEX', 'PERP_RACE')]
  
  sex_map <- c(M = "Male", F = "Female")
  demographic_data$PERP_SEX <- sex_map[demographic_data$PERP_SEX]
  
  total_arrests <- nrow(demographic_data)
  
  age_group_summary <- aggregate(ARREST_KEY ~ AGE_GROUP, data = demographic_data, FUN = length)
  age_group_summary$Percentage <- (age_group_summary$ARREST_KEY / total_arrests) * 100
  most_common_age_group <- age_group_summary[which.max(age_group_summary$ARREST_KEY), "AGE_GROUP"]
  least_common_age_group <- age_group_summary[which.min(age_group_summary$ARREST_KEY), "AGE_GROUP"]
  
  sex_summary <- aggregate(ARREST_KEY ~ PERP_SEX, data = demographic_data, FUN = length)
  sex_summary$Percentage <- (sex_summary$ARREST_KEY / total_arrests) * 100
  most_common_sex <- sex_summary[which.max(sex_summary$ARREST_KEY), "PERP_SEX"]
  
  race_summary <- aggregate(ARREST_KEY ~ PERP_RACE, data = demographic_data, FUN = length)
  race_summary$Percentage <- (race_summary$ARREST_KEY / total_arrests) * 100
  most_common_race <- race_summary[which.max(race_summary$ARREST_KEY), "PERP_RACE"]
  least_common_race <- race_summary[which.min(race_summary$ARREST_KEY), "PERP_RACE"]
  
  age_race_summary <- aggregate(ARREST_KEY ~ AGE_GROUP + PERP_RACE, data = demographic_data, FUN = length)
  most_common_age_race_combination <- age_race_summary[which.max(age_race_summary$ARREST_KEY), ]
  least_common_age_race_combination <- age_race_summary[which.min(age_race_summary$ARREST_KEY), ]
  
  insights_row <- data.frame(
    Most_Common_Age_Group = most_common_age_group,
    Least_Common_Age_Group = least_common_age_group,
    Most_Common_Sex = most_common_sex,
    Most_Common_Race = most_common_race,
    Least_Common_Race = least_common_race,
    Most_Common_Age_Race_Combination = paste(most_common_age_race_combination$AGE_GROUP, most_common_age_race_combination$PERP_RACE, sep = " - "),
    Least_Common_Age_Race_Combination = paste(least_common_age_race_combination$AGE_GROUP, least_common_age_race_combination$PERP_RACE, sep = " - ")
  )
  
  write.csv(insights_row, file="demographic_insights.csv", row.names=FALSE)
  write.csv(demographic_data, file="processed_demographic_data.csv", row.names=FALSE)
  
  faasr_put_file(local_file="demographic_insights.csv", remote_folder="demographic", remote_file="demographic_insights.csv")
  faasr_put_file(local_file="processed_demographic_data.csv", remote_folder="demographic", remote_file="processed_demographic_data.csv")
  
  log_msg <- paste0('Demographic data preprocessing complete; insights and data written to ', folder)
  faasr_log(log_msg)
}
