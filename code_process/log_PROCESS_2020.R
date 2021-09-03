#############
# Imports activity data from my 2020 spreadsheet. Activities are recorded on the "2020" sheet of the spreadsheet.
# Main output is saved to "log_2020.RDS"
#############

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2020.xlsx"

log_master <- read_excel(file_name, sheet="2020", range = cell_cols("A:U"))

# trim rows with no entry
log <- filter(log_master, !is.na(Type))

#Rename cols
colnames(log) <- c("Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Ave_power", "Norm_power", "Description", "Terrain", 
                   "Total_time", "Quick", "Feel", "Enjoy", "Physical_cost", "Mental_cost", 
                   "Feel_after", "Quality", "Quality_types", "Time_on", "Notes")

log <- mutate(log, Date=as.Date(Date)) #change Date to date object (from datetime)

# Replace NAs
log_2020 <- log %>%
  mutate(Total_time = ifelse(is.na(Total_time), Time, Total_time)) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate_if(is.character, ~replace_na(., "")) %>%
  rename_all(str_to_lower)

# Have left NAs in Description,  Quality_types, Notes

if(save_flag){
  saveRDS(log_master, "data_processed/log_2020_raw.RDS")
  saveRDS(log_2020, "data_processed/log_2020.RDS")
}