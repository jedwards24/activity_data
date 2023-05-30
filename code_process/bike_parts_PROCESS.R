# Process bike part maintenance to get distance used per part since
# last clean/repair/replace.
# Called from MASTER.R

library(tidyverse)
library(edwards)
library(lubridate)
source("functions.R")

events_load <- read_csv("data/bikes.csv", col_types = "cccc") %>%
  mutate(date = dmy(date))
dt <- readRDS("data_processed/log_all.RDS") %>%
  filter(type == "B")
dt2 <- readRDS("data_processed/log_2020.RDS") %>%
  bind_rows(readRDS("data_processed/log_2022.RDS")) %>%
  filter(type == "B", ave_power > 0)

new_dates <- tibble(bike = c("cgr", "cube", "4iiii", "scottmb"),
       date = c(ymd("2017-11-29"), ymd("2019-11-30"), ymd("2020-01-17"), ymd("2005-01-01")),
       event = "new")

standard_parts <- c("chain", "bottom bracket", "mech hanger", "rear gear cable",
                    "rear tube", "headset", "cassette", "freehub", "jockeys",
                    "front gear cable", "handlebar tape")

events <- expand_events(events_load, standard_parts, new_dates, bikes = c("cgr", "cube", "scottmb"))

# checks
if (F){
  events %>% filter(event == "new") %>% count_n(bike, part)
  events %>% filter(event == "now") %>% count_n(bike, part)
  events %>% filter(event == "retire") %>% count_n(bike, part)
}

events_full <- events %>%
  group_by(bike, part) %>%
  mutate(previous = lag(date)) %>%
  rowwise() %>%
  mutate(km_since_prev = part_distance(previous, date, bike, data = dt, power_data = dt2)) %>%
  ungroup() %>%
  mutate(days_since_previous = diff_days(date, previous)) %>%
  replace_na(list(days_since_previous = 0)) %>%
  select(-previous)

if(save_flag){
  saveRDS(events_full, "data_processed/bike_parts.RDS")
  cli::cli_alert_success("Saved data_processed/bike_parts.RDS")
}
