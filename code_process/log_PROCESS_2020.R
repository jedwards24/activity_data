#############
# Imports activity data from my 2020 spreadsheet. Activities are recorded on the "2020" sheet of the spreadsheet.
# Main output is saved to "log_2020.RDS"
#############

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2020.xlsx"

log_master <- read_excel(file_name, sheet="2020", range = cell_cols("A:T"))

# trim rows with no entry
log <- filter(log_master, !is.na(Type))

#Rename cols
colnames(log) <- c("Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Norm_power", "Description", "Terrain", 
                   "Total_time", "Quick", "Feel", "Enjoy", "Physical_cost", "Mental_cost", 
                   "Feel_after", "Quality", "Quality_types", "Time_on", "Notes")

log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)

# Replace NAs
log_2020 <- log %>%
  mutate(Total_time = ifelse(is.na(Total_time), Time, Total_time)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  replace_na(list(Subtype = "(none)")) %>% 
  rename_all(str_to_lower)

# Have left NAs in Description,  Quality_types, Notes

# Create new df with daily totals for each activity type (B, R, F) with  
# entries for all dates and types even if there was no activity of that type on that day.
totals_2020 <- log_2020 %>% 
  filter(type %in% c("B", "R", "F")) %>% 
  select(c(1:2, 4:6)) %>%
  group_by(date, type) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  arrange(date) %>%
  complete(date = seq.Date(ymd("2020-01-01"), today(), by = "days"),
           type = c("B", "F", "R"),
           fill = list(time = 0, distance = 0, ascent = 0))

if(save_flag){
  saveRDS(log_master, "data_processed/log_2020_raw.RDS")
  saveRDS(log_2020, "data_processed/log_2020.RDS")
  saveRDS(totals_2020, "data_processed/log_2020_totals.RDS")
}