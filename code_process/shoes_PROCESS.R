

library(tidyverse)
library(lubridate)

log20 <- readRDS("data_processed/log_2020.RDS")
log18 <- readRDS("data_processed/log_2018.RDS")
log13 <- readRDS("data_processed/log_2013.RDS")

# Combine dfs. The older df uses "notes" for the activity name
log18_simp <- log18 %>% 
  select_at(c(names(log13)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description)

log20_simp <- log20 %>% 
  select_at(c(names(log13)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description)

log_all <- bind_rows(log13,
                     mutate(log18_simp, week_data = 0),
                     mutate(log20_simp, week_data = 0)) %>%
  mutate(week_data = as.logical(week_data)) %>% 
  rename(name = notes)

# shoes ------------
shoe13 <- read_csv("data/shoes.csv", col_types = "icdl")

shoe_names <- select(shoe13, id, name, owned)

df13 <- shoe13 %>% 
  rename(distance = km_12_13) %>% 
  filter(distance > 0) %>% 
  mutate(year = 2013,
         type = "R",
         time = NA_real_,
         ascent = NA_real_)


shoes <- log_all %>%
  mutate(year = year(date)) %>% 
  filter(year >= 2014) %>% 
  filter(type %in% c("R", "F")) %>% 
  group_by(year, type, subtype) %>%
  summarise_at(.vars = c("time", "distance", "ascent"), sum) %>% 
  rename(id = subtype) %>% 
  mutate(id = na_if(id, "none")) %>%   
  mutate(id = as.integer(id)) %>% 
  full_join(shoe_names, by = "id") %>% 
  bind_rows(df13) %>% 
  arrange(id, year)

if (F){
  saveRDS(shoes, "data_processed/shoes.RDS")
}

# explore-----
shoes %>% 
  filter(type == "R") %>%
  filter(owned) %>% 
  group_by(id, name) %>% 
  summarise(distance = sum(distance)) %>% 
  arrange(desc(distance)) %>% 
  print(n = 20)

# summing F and R
shoes %>% 
  group_by(id) %>% 
  summarise(distance = sum(distance), name = unique(name)) %>% 
  arrange(desc(distance)) %>% 
  print(n = 25)

#just F
shoes %>% 
  filter(type == "F") %>% 
  group_by(id) %>% 
  summarise(distance = sum(distance), name = unique(name)) %>% 
  arrange(desc(distance))
