# Checks on data
# If I find obvious mistakes then I will correct them in the spreadsheet

library(tidyverse)
library(edwards)


log_all <- readRDS("data_processed/log_all.RDS")

log_all %>% 
  filter(distance == 0) %>% 
  filter(type %in% c("B", "R", "F"))

log_all %>% 
  filter(type %in% c("R", "F")) %>% 
  arrange(distance) %>% view()

count_nas(log_all)

