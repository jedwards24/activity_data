# Process bike part maintenance record to get summary per part.
# Called from MASTER.R

library(tidyverse)
library(edwards)
library(lubridate)
source("functions.R")

# Load data and basic checks -------------
ev <- read_csv("data/bike_parts.csv", col_types = "ccccc") %>%
  mutate(date = dmy(date)) %>%
  mutate(id = row_number())
new_events <- c("new", "replace", "front", "rear")

errs1 <- ev %>%
  filter(event %in% new_events, is.na(part)) %>%
  pull(id)
if (length(errs1) > 0){
  stop("All 'new', 'replace', 'front', 'rear' events must have a part name.\n",
       "The part name is missing in the following rows:\n", errs1)
}
suppressMessages(errs2 <- count_nas(select(ev, -part)))
if (length(errs2) > 0){
  stop("There must not be missing values in any column except 'part'.\n",
       "The following columns contain missing values:\n", names(errs2))
}

errs3 <- ev %>%
  filter(!is.na(part)) %>%
  group_by(category, part) %>%
  summarise(events = list(event), .groups = "drop") %>%
  mutate(new = map_lgl(events, ~any(new_events %in% .))) %>%
  filter(!new)
if (nrow(errs3) > 0){
  cat_parts <- mutate(errs3, cat_part = paste(category, part, sep = "-")) %>% pull(cat_part)
  stop("Each category/part combination must contain one of 'new', 'replace', 'front', or 'rear' events.\n",
       "The problems occurred in the following category-parts:\n", paste0(cat_parts, "\n"))
}

# Load other data ---------
# event data
ev <- ev %>%
  select(-id) %>%
  arrange(bike, category, date) %>%
  fill(part)
# Bike activity data
dt <- readRDS("data_processed/log_all.RDS") %>%
  filter(type == "B") %>%
  select(date, subtype, distance)
# Activities with power
dtp <- readRDS("data_processed/log_2020.RDS") %>%
  bind_rows(readRDS("data_processed/log_2022.RDS")) %>%
  filter(type == "B", ave_power > 0) %>%
  mutate(bike = "4iiii") %>%
  select(date, bike, distance)
# Auxiliary data tables
bike_tbl <- tibble(bike = c("cgr", "cube", "scottmb"), subtype = c("c", "a", "m"))
zero <- tibble(bike = c("4iiii", "cgr", "cube", "scottmb"),
               date = ymd("1900-01-01"),
               km = 0)

# Process "replace"
# Each replace event is replaced by two events (retire and new)
new_retire <- ev %>%
  arrange(bike, category, date) %>%
  fill(part) %>%
  group_by(bike, category) %>%
  mutate(prev = lag(part)) %>%
  filter(event == "replace") %>%
  pivot_longer(c(prev, part), values_to = "part") %>%
  mutate(event = ifelse(name == "part", "new", "retire")) %>%
  select(-name)
# Combine
ev2 <- ev %>%
  filter(event != "replace") %>%
  bind_rows(new_retire) %>%
  arrange(bike, category, date)

# Adding distance-----------
# Create cumulative km on original data and join by date
dt2 <- dt %>%
  left_join(bike_tbl, by = "subtype") %>%
  bind_rows(dtp) %>%
  filter(!is.na(bike)) %>%
  group_by(subtype) %>%
  mutate(km = cumsum(distance)) %>%
  ungroup() %>%
  select(-distance, -subtype)

ev_dates <- ev2 %>%
  select(date, bike) %>%
  distinct()

parts <- dt2 %>%
  bind_rows(zero) %>%
  bind_rows(ev_dates) %>%
  arrange(bike, date) %>%
  fill(km) %>%
  distinct() %>%
  right_join(ev2, by = c("bike", "date")) %>%
  relocate(part, .before = event) %>%
  arrange(bike, category, part, date) %>%
  relocate(km, .after = last_col())

# Current km/date---------
latest <- dt2 %>%
  group_by(bike) %>%
  summarise(latest_date = max(date),
            latest_km = max(km))

# Nest by part-----------
nest_parts <- parts %>%
  nest(data = c(date, event, km)) %>%
  mutate(active = map_lgl(data, ~!"retire" %in% .$event)) %>%
  mutate(start_date = as_date(map_dbl(data, ~min(.$date)))) %>%
  mutate(last_date = as_date(map_dbl(data, ~max(.$date)))) %>%
  arrange(bike, category, start_date) %>%
  mutate(start_km = map_dbl(data, ~min(.$km))) %>%
  mutate(last_km = map_dbl(data, ~max(.$km))) %>%
  left_join(latest, by = "bike") %>%
  mutate(latest_date = if_else(active, latest_date, last_date)) %>%
  mutate(latest_km = if_else(active, latest_km, last_km)) %>%
  mutate(total_km = latest_km - start_km) %>%
  mutate(total_days = diff_days(latest_date, start_date)) %>%
  mutate(last_event = map_chr(data, ~last(.$event))) %>%
  mutate(km_since_last = latest_km - map_dbl(data, ~last(.$km))) %>%
  mutate(days_since_last = diff_days(latest_date, as_date(map_dbl(data, ~last(.$date))))) %>%
  mutate(front_km = map2_dbl(data, latest_km, tyre_front_km)) %>%
  mutate(rear_km = total_km - front_km) %>%
  select(-latest_date, -latest_km, -start_km, -last_km) %>%
  relocate(event_data = data, .after = last_col())

attr(nest_parts, "latest_date") <- max(latest$latest_date)

if(save_flag){
  saveRDS(nest_parts, "data_processed/bike_parts.RDS")
  cli::cli_alert_success("Saved data_processed/bike_parts.RDS")
}
