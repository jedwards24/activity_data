#############
# Explores raw data before processing.
# prob won't be kept. Convert to notes before deleting.
#############

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)
source("activity_FUNC.R")

# Data sheets
# 2013: Log, Stress&Health, Workout Record, TL nea, TL new out, Stresses, Sleep, Records, TL old
# 2013 but covered by 2018: Record Progression, Races, SAM
# 2018: 2018, Stress&Health, VV, Sleep, SAM, Perform
# 2018 but covered by 2020: Record progression, Races, Weight
# 2020: 2018, Stress&Health, Records, Races, Weight, SAM
# SAM 2020 is new and different from previous SAM

file_name <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record.xlsx"

excel_sheets(file_name) %>% datapasta::dpasta()
c("Log", "Stress&Health", "Shoes", "Workout Record", "Routes", "Summary - months", "Summary - weeks", "Record progression", "TL new", "TL new out", "Races", "Rides&runs", "Stresses", "Sleep", "workouts_print", "aux_print", "Records", "Health measures", "TL old", "TL output", "print", "Workout (old)", "Health print", "SAM")
dt <- read_excel(file_name, sheet = "Stress&Health", skip = 2)


file_name2 <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2018.xlsx"

excel_sheets(file_name2)

file_name3 <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2020.xlsx"


# stress& health -------------
dt <- read_excel(file_name2, sheet = "Stress&Health", skip = 2)
var_summary(dt2)
dt %>% select(15:17) %>% distinct() %>% prinf
dt2 <- dt %>%
  select(1:14) %>% 
  rename_all(~str_remove_all(., "[\\d-()&]")) %>%
  rename_all(str_squish) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  rename_all(str_to_lower)

# 2020
dt <- read_excel(file_name3, sheet = "Stress&Health", skip = 2)
var_summary(dt)
dt %>% select(15:17) %>% distinct() %>% prinf
dt2 <- dt %>%
  select(1:14) %>% 
  rename_all(~str_remove_all(., "[\\d-()&]")) %>%
  rename_all(str_squish) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  rename_all(str_to_lower) %>% 
  filter(!is.na(energy) & !is.na(mood)) %>% 
  mutate(date = as_date(date))
dt2
month_ener <- dt2 %>% 
  mutate(month = factor(month(date, label = T), ordered = T)) %>%
  group_by(month) %>% 
  count2(energy, sort = F) %>% 
  filter(sum(n) > 10) %>% 
  ungroup() %>% 
  complete(nesting(month), energy, fill = list(n = 0L, prop = 0)) %>% 
  mutate(energy = factor(energy, ordered = T)) 

month_ener %>% 
  pivot_wider(month, names_from = energy, values_from = prop, names_prefix = "energy_")  %>% 
  replace_na(list(energy_1 = 0, energy_2 = 0, energy_3 = 0))  

month_ener %>% 
  ggplot(aes(month, prop, group = energy, colour = energy)) +
  geom_line() +
  geom_point() +
  theme_minimal()

# weight --------------

dtw <- read_excel(file_name3, sheet = "Weight", skip = 0, guess_max = 1000, range = cell_cols(1:10)) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  rename_all(str_to_lower) %>% 
  rename_all(~str_remove_all(., "_%")) %>% 
  rename(weight = `weight_(kg)`) %>% 
  mutate(date = as.Date(date))

ggplot(dtw, aes(x = date, y = weight)) +
  geom_point() +
  geom_smooth()
qplot(x = date, y = weight, data = dtw, geom = "smooth") + geom_hline(yintercept = mean(dtw$weight))
