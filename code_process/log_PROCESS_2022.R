# Imports activity data from my 2022 spreadsheet (on Google sheets).
# Activities are recorded on the "Record" sheet of the spreadsheet.
# Main output is saved to "log_2022.RDS"
#############

library(tidyverse)
library(googlesheets4)
library(edwards)
library(lubridate)

new_names <-  c("date", "type", "subtype", "time", "distance", "ascent", "ave_power",
                "norm_power", "description", "terrain", "total_time", "quick", "feel",
                "enjoy", "physical_cost", "mental_cost", "feel_after", "quality",
                "quality_types", "time_on", "notes")

log_master <- read_sheet("1w9vNdPAB3qxqnqbToHmGBjxSSAN0-ckYuK_96oluSG8",
                 col_types = "Dccnnnnncnnnnnnnnncnc")

log_2022 <- filter(log_master, !is.na(Type)) %>%
  setNames(new_names) %>%
  log_replace_nas()

# Have left NAs in Description,  Quality_types, Notes

if(save_flag){
  saveRDS(log_master, "data_processed/log_2022_raw.RDS")
  saveRDS(log_2022, "data_processed/log_2022.RDS")
}
