# Create totals of activities by day for each of R, B, F and save

library(tidyverse)
library(edwards)
source("functions.R")

t13 <- readRDS("data_processed/log_2013.RDS") %>% 
  total_by_day(ymd("2013-07-01"), ymd("2017-12-31"))
t18 <- readRDS("data_processed/log_2018.RDS") %>% 
  total_by_day(ymd("2018-01-01"), ymd("2019-12-31"))
t20 <- readRDS("data_processed/log_2020.RDS") %>% 
  total_by_day(ymd("2020-01-01"), today())

tot_all <- bind_rows(t13, t18, t20)

if (save_flag){
  saveRDS(tot_all, "data_processed/totals.RDS")
}