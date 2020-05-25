#############
# Explores raw data before processing.
# prob won't be kept. Convert to notes before deleting.
#############

library(tidyverse)
library(readxl)
library(edwards)
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
dt <- read_excel(file_name, sheet = "Stress&Health", skip = 2)


file_name2 <- "C:/Users/James/Dropbox/Mine/Personal/Activity Record 2018.xlsx"

excel_sheets(file_name2)

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

# weight --------------
dt <- read_excel(file_name2, sheet = "Weight", skip = 0) %>%
  rename(notes = `...9`) %>% 
  rename_all(~str_replace_all(., " ", "_")) %>% 
  rename_all(str_to_lower) %>% 
  rename_all(~str_remove_all(., "_%")) %>% 
  rename(weight = `weight_(kg)`) %>% 
  mutate(date = as.Date(date))

ggplot(dt, aes(x = date, y = weight)) +
  geom_point() +
  geom_smooth()
qplot(x = date, y = weight, data = dt, geom = "smooth")
