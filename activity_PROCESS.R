#############
# Imports activity data from my spreadsheet. Activities are recorded on the "log" sheet of the spreadsheet.
# See "activity_EXPLORE.R" for summary of dfs created.
#############

library(tidyverse)
library(readxl)
library(edwards)
file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record.xlsx"

source("activity_FUNC.R")

excel_sheets(file_name)
dt <- read_excel(file_name, sheet="Log", skip=12)
glimpse(dt)
var_summary(dt)

#trim rows with no entry and remove blank column
log <- dt %>% 
  filter(!is.na(Type)) %>%
  select(-c(`...16`))

#Rename cols
colnames(log)
colnames(log) <- c("Week_total", "Date", "Type", "Subtype", "Time", "Distance", "Ascent", "Notes", "Terrain", 
                   "Tempo_pace", "5k10k_pace", "Sub_5k_pace", "Hill_sprints", "Strides", "Drills", 
                   "Total_time", "Year", "Month", "Week")

#glimpse(log)
log <- mutate(log, Date = as.Date(Date)) #change Date to date object (from datetime)

# List number of NAs and replace
count_nas(log)
var_summary(log)

log <- log %>%
  rename_all(str_to_lower) %>% 
  mutate(total_time = ifelse(is.na(total_time), time, total_time)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  replace_na(list(week_total = 0, 
                  subtype = "none",
                  notes = "none")) %>%
  mutate(week_total = ifelse(week_total == "week", 1, 0))  # Convert week total to binary 

#New dfs separated by activity type (loses day, week, month columns)
log_R <- log %>% filter(type=="R") %>%
  select(c(1:16))
log_B <- log %>% filter(type=="B") %>%
  select(c(1:8, 16))
log_F <- log %>% filter(type=="F") %>%
  select(c(1:9, 16))

#Split week totals (loses terrain)
#Note 5 day week for cycling
# NAs created in Notes column by split_week_data function
log_B_split <- log_B %>% 
  split_week_data(max_week=5) %>%
  mutate(notes = replace_na(notes, "none"))

log_F_split <- log_F %>% 
  select(-terrain) %>%
  split_week_data(max_week=7) %>%
  mutate(notes = replace_na(notes, "none"))

#Checks
if (F){
  log_B_split %>% summarise_if(is.numeric, sum)
  log_B %>% summarise_if(is.numeric, sum)
  
  log_F_split %>% summarise_if(is.numeric, sum)
  log_F %>% summarise_if(is.numeric, sum)
  
  count_nas(log_F_split)
  count_matches(log_F_split, "none")
}

#Combine R,B,F into single df
log_new <- log_R %>% 
  rename(week_data = week_total) %>% 
  select(c(1:8, 16)) %>%
  bind_rows(log_B_split) %>% 
  bind_rows(log_F_split) %>% 
  arrange(date)

#Create temp df with zero entries for all days and types
start <- min(log_new$date)
ndays <- as.numeric(max(log_new$date) - start) + 1
nn <- 3 * ndays
temp_df <- tibble(date = rep(start + (1 : ndays) - 1, 3),
                  type = rep(c("B", "F", "R"), each=ndays),
                  time = 0,
                  distance = 0,
                  ascent = 0)

# Create new df with daily totals for each activity type (uses temp_df to add zero 
# entries on days where there is no activity for a given type)
totals <- log_new %>% 
  select(c(2:3, 5:7)) %>%
  bind_rows(temp_df) %>%
  group_by(type, date) %>% 
  summarise_all(sum)

totals <- totals %>% 
  ungroup() %>% 
  mutate(day=as.numeric(date - min(date) + 1)) %>%
  arrange(date)

# save
if(F){
  saveRDS(dt, "data_processed/log_master.RDS")
  saveRDS(log, "data_processed/log_clean.RDS")
  saveRDS(log_B, "data_processed/log_B.RDS")
  saveRDS(log_R, "data_processed/log_R.RDS")
  saveRDS(log_F, "data_processed/log_F.RDS")
  saveRDS(log_new, "data_processed/log_new.RDS")
  saveRDS(totals, "data_processed/log_totals.RDS")
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
