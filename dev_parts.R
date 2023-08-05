# redo parts processing
library(tidyverse)
library(edwards)
library(lubridate)
source("functions.R")

ev <- read_csv("data/bike_parts.csv", col_types = "ccccc") %>%
  mutate(date = dmy(date))
ev %>%
  arrange(bike, category, part)

# fill down missing parts
ev2 <- ev %>%
  arrange(bike, category, date) %>%
  fill(part) %>% prinf

ev %>%
  arrange(bike, category, date) %>%
  nest(data = c(part, event, date))

# Process "replace"
# Each replace event is replaced by two events (retire and new)

# test
tt <- ev2 %>%
  arrange(bike, category, date) %>%
  fill(part) %>%
  filter(bike == "4iiii")
tt %>%
  mutate(prev = lag(part)) %>%
  filter(event == "replace") %>%
  pivot_longer(c(prev, part), values_to = "part") %>%
  mutate(event = ifelse(name == "part", "new", "retire")) %>%
  select(-name)

#everything
new_rows <- ev2 %>%
  arrange(bike, category, date) %>%
  fill(part) %>%
  group_by(bike, category) %>%
  mutate(prev = lag(part)) %>%
  filter(event == "replace") %>%
  pivot_longer(c(prev, part), values_to = "part") %>%
  mutate(event = ifelse(name == "part", "new", "retire")) %>%
  select(-name)
# Combine
ev3 <- ev2 %>%
  filter(event != "replace") %>%
  bind_rows(new_rows) %>%
  arrange(bike, category, date)
prinf(ev3)

# Adding distance-----------
# Could use original method
# OR create cumulative km on original data and join by date
dt <- readRDS("data_processed/log_all.RDS") %>%
  filter(type == "B")
dtp <- readRDS("data_processed/log_2020.RDS") %>%
  bind_rows(readRDS("data_processed/log_2022.RDS")) %>%
  filter(type == "B", ave_power > 0) %>%
  mutate(bike = "4iiii") %>%
  select(date, bike, distance)


bike_lu <- tibble(bike = c("cgr", "cube", "scottmb"), subtype = c("c", "a", "m"))
dt2 <- dt %>%
  select(date, subtype, distance) %>%
  left_join(bike_lu, by = "subtype") %>%
  bind_rows(dtp) %>%
  filter(!is.na(bike)) %>%
  group_by(subtype) %>%
  mutate(km = cumsum(distance)) %>%
  ungroup()
zero <- tibble(bike = unique(dt2$bike),
               date = ymd("1900-01-01"),
               km = 0)
ev4 <- ev3 %>%
  select(date, bike) %>%
  distinct()

dt3 <- dt2 %>%
  select(-distance, -subtype) %>%
  bind_rows(zero) %>%
  bind_rows(ev4) %>%
  arrange(bike, date) %>%
  fill(km) %>%
  distinct()
dt3
parts <- ev3 %>%
  left_join(dt3, by = c("bike", "date")) %>%
  relocate(part, .before = event) %>%
  arrange(bike, category, part, date)
prinf(parts)

# Current km/date---------
latest <- dt2 %>%
  group_by(bike) %>%
  summarise(latest_date = max(date),
            latest_km = max(km))
# Nest -----------
nestp <- parts %>%
  nest(data = c(date, event, km)) %>%
  mutate(start_date = as_date(map_dbl(data, ~min(.$date)))) %>%
  mutate(last_date = as_date(map_dbl(data, ~max(.$date)))) %>%
  arrange(bike, category, start_date) %>%
  mutate(active = map_lgl(data, ~!"retire" %in% .$event)) %>%
  mutate(start_km = map_dbl(data, ~min(.$km))) %>%
  mutate(last_km = map_dbl(data, ~max(.$km))) %>%
  left_join(latest, by = "bike") %>%
  mutate(total_km = if_else(active, latest_km - start_km, last_km - start_km)) %>%
  mutate(total_days = if_else(active, diff_days(latest_date, start_date), diff_days(last_date, start_date)))
nestp %>%
  select(-data, -latest_date, -latest_km) %>%
  prinf()

nestp %>%
  filter(bike == "cube") %>%
  select(-data, -latest_date, -latest_km)

nestp %>%
  filter(category == "chain") %>%
  select(-data, -latest_date, -latest_km)

# tyre summary---------------
# Possible event patterns:
# fr, frx, fx, f  (x = retire)
# could calculate km on front then get rest by subtracting

nestp %>%
  filter(category == "tyre") %>%
  slice(3) %>%
  unnest(data)

# km that tyre has done on front
# latest_km is current bike km
tyre_front_km <- function(dat, latest_km) {
  ev <- dat$event
  km <- dat$km
  active = !"retire" %in% ev
  last_km <- if (active) latest_km else km[ev == "retire"]
  if ("to rear" %in% ev) return(km[ev == "to rear"] - km[ev == "front"])
  if (identical("front", ev)) return(latest_km - km[1])
  if ("to front" %in% ev) return(last_km - km[ev == "to front"])
  0
}

nestp %>%
  filter(category == "tyre") %>%
  mutate(front_km = map2_dbl(data, latest_km, tyre_front_km)) %>%
  mutate(rear_km = total_km - front_km) %>%
  select(-data, -latest_date, -latest_km)


#############
slice(ev3, 50)
dt3 %>%
  filter(bike == "cgr", date == ymd("2021-01-22"))
slice(dt3, 246)
ev3 %>%
  filter(bike == "cgr", date == ymd("2020-10-25"))

# Can't join by date bc dates not complete
# Most efficient: add new rows in dt to match events dates (fill down)
# Need an inital zero row for each bike

