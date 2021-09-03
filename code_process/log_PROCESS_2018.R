#############
# Imports activity data from my 2018 spreadsheet. Activities are recorded on the "2018" sheet of the spreadsheet.
# Main output is df "log_2018_proc"
#############
library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2018.xlsx"

log_master <- read_excel(file_name, sheet="2018", range = cell_cols("A:AA"))

# trim rows with no entry and remove blank columns
log <- filter(log_master, !is.na(Type))

#Rename cols
colnames(log) <- c("Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Description", "Terrain", 
                   "Total_time", "Strides", "SAM", "Feel", "Enjoy", "Physical_cost", "Mental_cost", 
                   "Feel_after", "Quality", "Quality_types", "Workout_type", "Time_hard", "Time_mod", 
                   "Density", "Hilly", "Total_work", "Time_on", "Recovery", "Notes")

log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)

# Replace NAs
log_2018 <- log %>%
  mutate(Total_time = ifelse(is.na(Total_time), Time, Total_time)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate_if(is.character, ~replace_na(., "")) %>%
  rename_all(str_to_lower)

# Have left NAs in Description,  Quality_types, Workout_type, Notes

if(save_flag){
  saveRDS(log_master, "data_processed/log_2018_raw.RDS")
  saveRDS(log_2018, "data_processed/log_2018.RDS")
}