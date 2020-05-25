# Merge and save the processed log and total data files
# Only columns common to all logs are retained.
library(tidyverse)
library(edwards)

t13 <- readRDS("data_processed/log_2013_totals.RDS")
t18 <- readRDS("data_processed/log_2018_totals.RDS")
t20 <- readRDS("data_processed/log_2020_totals.RDS")

tot <- bind_rows(t13, t18, t20)

if (save_flag){
  saveRDS(tot, "data_processed/totals.RDS")
}

log13 <- readRDS("data_processed/log_2013.RDS")
log18 <- readRDS("data_processed/log_2018.RDS")
log20 <- readRDS("data_processed/log_2020.RDS")

# Combine dfs. The older df uses "notes" for the activity name
log18_simp <- log18 %>% 
  select_at(c(names(log13)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description) %>% 
  mutate(week_data = 0)

log20_simp <- log20 %>% 
  select_at(c(names(log13)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description) %>% 
  mutate(week_data = 0)

log_all <-
  bind_rows(log13, log18_simp, log20_simp) %>% 
  mutate(week_data = as.logical(week_data)) %>% 
  rename(name = notes) %>% 
  select( c(2:9, 1))

if (save_flag){
  saveRDS(log_all, "data_processed/log_all.RDS")
}

