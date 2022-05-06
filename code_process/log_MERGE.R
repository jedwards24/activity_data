# Merge and save the processed log and total data files
# Only columns common to all logs are retained.
library(tidyverse)
library(edwards)

log13 <- readRDS("data_processed/log_2013.RDS")
log18 <- readRDS("data_processed/log_2018.RDS")
log20 <- readRDS("data_processed/log_2020.RDS")
log22 <- readRDS("data_processed/log_2022.RDS")

# Combine dfs using only columns common to all. The older df uses "notes" for the activity name
keep_cols <- setdiff(names(log13), c("week_data", "notes")) %>%
  c("description")

log18_simp <- log_simplify(log18, keep_cols)
log20_simp <- log_simplify(log20, keep_cols)
log22_simp <- log_simplify(log22, keep_cols)

log_all <-
  bind_rows(log13, log18_simp, log20_simp, log22_simp) %>%
  mutate(week_data = as.logical(week_data)) %>%
  rename(name = notes) %>%
  relocate(week_data, .after = last_col())

if (save_flag){
  saveRDS(log_all, "data_processed/log_all.RDS")
}

