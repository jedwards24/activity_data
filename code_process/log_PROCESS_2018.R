#############
# Imports activity data from my 2018 spreadsheet. Activities are recorded on the "2018" sheet of the spreadsheet.
# Main output is df "log_2018_proc"
#############
library(tidyverse)
library(readxl)
library(edwards)

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
  replace_na(list(Subtype = "none")) %>% 
  rename_all(str_to_lower)

# Left NAs in Description,  Quality_types, Workout_type, Notes

# Create new df with daily totals for each activity type (uses temp_df to add zero 
# entries on days where there is no activity for a given type)
# Uses temp df with zero entries for all days and types
start <- min(log_2018$date)
ndays <- as.numeric(max(log_2018$date) - start) + 1
temp_df <- tibble(date = rep(start + (1 : ndays) - 1, 3), 
                  type = rep(c("B", "F", "R"), 1, each=ndays),
                  time = 0,
                  distance = 0,
                  ascent = 0)

totals_2018 <- log_2018 %>% 
  select(c(1:2, 4:6)) %>%
  bind_rows(temp_df) %>% 
  group_by(type, date) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  mutate(day=as.numeric(date - min(date) + 1)) %>%
  arrange(date)

if(F){
  saveRDS(log_master, "data_processed/log_2018_raw.RDS")
  saveRDS(log_2018, "data_processed/log_2018.RDS")
  saveRDS(totals_2018, "data_processed/log_2018_totals.RDS")
}