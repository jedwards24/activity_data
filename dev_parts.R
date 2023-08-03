# redo parts processing
library(tidyverse)
library(edwards)
library(lubridate)
source("functions.R")

ev <- read_csv("data/bike_parts.csv", col_types = "ccccc") %>%
  mutate(date = dmy(date))
ev %>%
  arrange(bike, category, part)
ev %>%
  group_by(bike, category) %>%
  nest()
?nest
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
ev2 %>%
  filter(event != "replace") %>%
  bind_rows(new_rows) %>%
  arrange(bike, category, date) %>%
  prinf()

# Adding distance-----------
# Could use original method
# OR create cumulative km on original data and join by date
dt <- readRDS("data_processed/log_all.RDS") %>%
  filter(type == "B")
bike_lu <- tibble(bike = c("cgr", "cube", "scottmb"), subtype = c("c", "a", "m"))
dt2 <- dt %>%
  select(date, subtype, distance) %>%
  left_join(bike_lu, by = "subtype") %>%
  filter(!is.na(bike)) %>%
  group_by(subtype) %>%
  mutate(km = cumsum(distance))
dt2 %>%
  filter(subtype == "m") %>% prinf()
# Can't join by date bc dates not complete
# Most efficient: add new rows in dt to match events dates (fill down)
# Need an inital zero row for each bike

# Checks needed:----------
# Any replace/new/front/rear should have a part name
# Missing values (except part)
# Each category/part should have one of new/replace/front/rear
