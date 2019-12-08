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
colnames(log) <- c("Week_total", "Date", "Type", "Sub-type", "Time", "Distance", "Ascent", "Notes", "Terrain", 
                   "Tempo_pace", "5k10k_pace", "Sub_5k_pace", "Hill_sprints", "Strides", "Drills", 
                   "Total_time", "Year", "Month", "Week")

#glimpse(log)
log <- mutate(log, Date = as.Date(Date)) #change Date to date object (from datetime)

# List number of NAs and replace
count_nas(log)
var_summary(log)

log <- log %>%
  mutate(Total_time = ifelse(is.na(Total_time), Time, Total_time)) %>% 
  mutate_if(is.numeric, ~replace_na(., 0)) %>% 
  replace_na(list(Week_total = 0, 
                  `Sub-type` = "none",
                  Notes = "none")) %>%
  mutate(Week_total = ifelse(Week_total == "week", 1, 0)) # Convert week total to binary


#New dfs separated by activity type (loses day, week, month columns)
log_R <- log %>% filter(Type=="R") %>%
  select(c(1:16))
log_B <- log %>% filter(Type=="B") %>%
  select(c(1:8, 16))
log_F <- log %>% filter(Type=="F") %>%
  select(c(1:9, 16))

#Split week totals (loses terrain)
#Note 5 day week for cycling
# NAs created in Notes column by split_week_data function
log_B_split <- log_B %>% 
  split_week_data(max_week=5) %>%
  mutate(Notes = replace_na(Notes, "none"))

log_F_split <- log_F %>% 
  select(-Terrain) %>%
  split_week_data(max_week=7) %>%
  mutate(Notes = replace_na(Notes, "none"))

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
  rename(Week_data = Week_total) %>% 
  select(c(1:8, 16)) %>%
  bind_rows(log_B_split) %>% 
  bind_rows(log_F_split) %>% 
  arrange(Date)

#Create temp df with zero entries for all days and types
start <- min(log_new$Date)
ndays <- as.numeric(max(log_new$Date) - start) + 1
nn <- 3 * ndays
temp_df <- log_new[1: nn, ]
temp_df$Date <- rep(start + (1 : ndays) - 1, 3)
temp_df$Type <- rep(c("B", "F", "R"), 1, each=ndays)
temp_df[, 5:7] <- 0
temp_df <- temp_df %>% select(c(2:3, 5:7))

#Create new df with daily totals for each activity type (uses temp_df to add zero 
# entries on days where there is no activity for a given type)
totals <- log_new %>% 
  select(c(2:3, 5:7)) %>%
  bind_rows(temp_df) %>% 
  group_by(Type, Date) %>% 
  summarise_all(funs(sum))

totals <- totals %>% 
  ungroup() %>% 
  mutate(Day=as.numeric(Date - min(Date) + 1)) %>%
  arrange(Date)

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
  totals %>% group_by(Type) %>% summarise_if(is.numeric, sum)
  sum(1:ndays)
  temp_df %>% arrange(Date)
  totals %>% group_by(Date) %>% tally
  
  log_new %>% select(Date, Type) %>% unique %>% tally
  length(unique(totals$Day))
  glimpse(totals)
}

