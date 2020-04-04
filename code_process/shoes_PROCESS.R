

library(tidyverse)
library(lubridate)

log_all <- readRDS("data_processed/log_all.RDS")

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
  mutate(id = na_if(id, "(none)")) %>%
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

#2020
shoes %>% 
  filter(year == 2020) %>% 
  filter(type == "R") %>% 
  group_by(id) %>% 
  summarise(distance = sum(distance), name = unique(name)) %>% 
  arrange(desc(distance))
  