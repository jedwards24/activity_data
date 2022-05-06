#############
# Imports activity data from my 2020 spreadsheet. Activities are recorded on the "2020" sheet of the spreadsheet.
# Main output is saved to "log_2020.RDS"
#############

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

#file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2020.xlsx"
file_name <- file.path(data_path, "Activity record 2020.xlsx")

log_master <- read_excel(file_name, sheet="2020", range = cell_cols("A:U"))

new_names <-  c("date", "type", "subtype", "time", "distance", "ascent", "ave_power",
                "norm_power", "description", "terrain", "total_time", "quick", "feel",
                "enjoy", "physical_cost", "mental_cost", "feel_after", "quality",
                "quality_types", "time_on", "notes")

log_2020 <- filter(log_master, !is.na(Type)) %>%
  setNames(new_names) %>%
  mutate(date=as.Date(date)) %>%
  log_replace_nas()

# Have left NAs in Description,  Quality_types, Notes

if(save_flag){
  saveRDS(log_master, "data_processed/log_2020_raw.RDS")
  saveRDS(log_2020, "data_processed/log_2020.RDS")
}
