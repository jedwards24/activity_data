#############
# Imports activity data from my 2018 spreadsheet. Activities are recorded on the "2018" sheet of the spreadsheet.
# Output is df "log_2018"
#############
library(tidyverse)
library(readxl)
library(edwards)
file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2018.xlsx"

#readxl has two functions
excel_sheets(file_name)
log_master <- read_excel(file_name, sheet="2018", range = cell_cols("A:AA"))

#glimpse(log_master)
#count(log_master, is.na(Type))

# trim rows with no entry and remove blank columns
log <- filter(log_master, !is.na(Type))
#Rename cols
colnames(log)
colnames(log) <- c("Date", "Type", "Sub-type", "Time", "Distance", "Ascent", "Description", "Terrain", 
                   "Total_time", "Strides", "SAM", "Feel", "Enjoy", "Physical_cost", "Mental_cost", 
                   "Feel_after", "Quality", "Quality_types", "Workout_type", "Time_hard", "Time_mod", 
                   "Density", "Hilly", "Total_work", "Time_on", "Recovery", "Notes")

#glimpse(log)
log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)

# List number of NAs and replace
count_nas(log)

log_2018 <- log %>%
  mutate(Total_time = ifelse(is.na(Total_time), Time, Total_time)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  replace_na(list(`Sub-type` = "none"))

count_nas(log_2018)
# Left NAs in Description,  Quality_types, Workout_type, Notes

workouts <- filter(log_2018, Quality >= 1)

#Create new df with daily totals for each activity type (uses temp_df to add zero 
# entries on days where there is no activity for a given type)
#Create temp df with zero entries for all days and types
start <- min(log_2018$Date)
ndays <- as.numeric(max(log_2018$Date) - start) + 1
temp_df <- tibble(Date = rep(start + (1 : ndays) - 1, 3), 
                  Type = rep(c("B", "F", "R"), 1, each=ndays),
                  Time = 0,
                  Distance = 0,
                  Ascent = 0)

totals_2018 <- log_2018 %>% 
  select(c(1:2, 4:6)) %>%
  bind_rows(temp_df) %>% 
  group_by(Type, Date) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  mutate(Day=as.numeric(Date - min(Date) + 1)) %>%
  arrange(Date)
var_summary(totals_2018)

if(F){
  saveRDS(log_master, "data_processed/2018_master.RDS")
  saveRDS(log_2018, "data_processed/2018_clean.RDS")
  saveRDS(totals_2018, "data_processed/2018_totals.RDS")
}