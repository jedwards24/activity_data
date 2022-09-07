#############
# Imports activity data from my 2018 spreadsheet. Activities are recorded on the "2018" sheet of the spreadsheet.
# Main output is df "log_2018_proc"
#############
library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

#file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2018.xlsx"
file_name <- file.path(data_path, "Activity record 2018.xlsx")

log_master <- read_excel(file_name, sheet="2018", range = cell_cols("A:AA"))

names_2018 <- c("Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Description", "Terrain",
                "Total_time", "Strides", "SAM", "Feel", "Enjoy", "Physical_cost", "Mental_cost",
                "Feel_after", "Quality", "Quality_types", "Workout_type", "Time_hard", "Time_mod",
                "Density", "Hilly", "Total_work", "Time_on", "Recovery", "Notes")

log_2018 <- filter(log_master, !is.na(Type)) %>%
  setNames(str_to_lower(names_2018)) %>%
  mutate(date=as.Date(date)) %>%
  log_replace_nas()

# Have left NAs in Description,  Quality_types, Workout_type, Notes

if(save_flag){
  saveRDS(log_master, "data_processed/log_2018_raw.RDS")
  saveRDS(log_2018, "data_processed/log_2018.RDS")
}
