#############
# Imports activity data from my spreadsheet. Activities are recorded on the "log" sheet of the spreadsheet.
# See "activity_EXPLORE.R" for summary of dfs created.
#############

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)
file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record.xlsx"

source("activity_FUNC.R")

dt <- read_excel(file_name, sheet="Log", skip=12)

#trim rows with no entry and remove blank column
log <- dt %>% 
  filter(!is.na(Type)) %>%
  select(-c(`...16`))

#Rename cols
colnames(log) <- c("Week_total", "Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Notes", "Terrain", 
                   "Tempo_pace", "5k10k_pace", "Sub_5k_pace", "Hill_sprints", "Strides", "Drills", 
                   "Total_time", "Year", "Month", "Week")

log <- mutate(log, Date = as.Date(Date)) #change Date to date object (from datetime)

# rename and replace NAs

log <- log %>%
  rename_all(str_to_lower) %>% 
  mutate(total_time = ifelse(is.na(total_time), time, total_time)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  replace_na(list(week_total = 0, 
                  subtype = "(none)",
                  notes = "(none)")) %>%
  mutate(week_total = ifelse(week_total == "week", 1, 0))  # Convert week total to binary 

#New dfs separated by activity type (loses day, week, month columns)
log_R <- log %>% filter(type=="R") %>%
  select(c(1:16))
log_B <- log %>% filter(type=="B") %>%
  select(c(1:8, 16))
log_F <- log %>% filter(type=="F") %>%
  select(c(1:9, 16))


# Split week totals (loses terrain)
# Note 5 day week for cycling
# NAs created in Notes column by split_week_data function
log_B_split <- log_B %>% 
  split_week_data(max_week=5) %>%
  mutate(notes = replace_na(notes, "(none)")) %>% 
  filter(distance != 0)

log_F_split <- log_F %>% 
  select(-terrain) %>%
  split_week_data(max_week=7) %>%
  mutate(notes = replace_na(notes, "(none)")) %>% 
  filter(distance != 0)

#Checks
if (F){
  log_B_split %>% summarise_if(is.numeric, sum)
  log_B %>% summarise_if(is.numeric, sum)

  log_F_split %>% summarise_if(is.numeric, sum)
  log_F %>% summarise_if(is.numeric, sum)
  
  count_nas(log_F_split)
  count_matches(log_F_split, "(none)")
}

#Combine R,B,F into single df
log_new <- log_R %>% 
  rename(week_data = week_total) %>% 
  select(c(1:8, 16)) %>%
  bind_rows(log_B_split) %>% 
  bind_rows(log_F_split) %>% 
  arrange(date)

# Create new df with daily totals for each activity type (B, R, F) with  
# entries for all dates and types even if there was no activity of that type on that day.
totals <- log_new %>% 
  filter(type %in% c("B", "R", "F")) %>% 
  select(c(2:3, 5:7)) %>%
  group_by(date, type) %>% 
  summarise_all(sum) %>%
  ungroup() %>% 
  arrange(date) %>%
  complete(date = seq.Date(ymd("2013-07-01"), ymd("2017-12-31"), by = "days"),
           type = c("B", "F", "R"),
           fill = list(time = 0, distance = 0, ascent = 0))

# save
if(save_flag){
  saveRDS(dt, "data_processed/log_2013_raw.RDS")
  saveRDS(log, "data_processed/log_2013_clean.RDS")
  saveRDS(log_new, "data_processed/log_2013.RDS")
  saveRDS(totals, "data_processed/log_2013_totals.RDS")
}

#checks
if(F){
  totals %>% group_by(type) %>% summarise_if(is.numeric, sum)
  sum(1:ndays)
  temp_df %>% arrange(date)
  totals %>% group_by(date) %>% tally
  
  log_new %>% select(date, type) %>% unique %>% tally
  length(unique(totals$day))
  glimpse(totals)
}
