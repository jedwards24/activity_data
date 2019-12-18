############
# Explore previously processed and saved RDS files.
############
library(tidyverse)
library(lubridate)
source("activity_FUNC.R")

#log_master - the original data import
#log - the original data with blank rows removed and some other cleaning
#log_B, log_R, log_F - the data for types B,F,R with appropriate variables
#log_B_split, log_F_split - B and F data with week data split up over following week
#log_new - R combined with B and F splits. 9 variables so some running specific ones lost (paces, terrain, hs, strides)
#totals - Daily totals for each of R,B,F (time, distance, ascent)
#
# 2018_clean.RDS is the main 2018 file
log_2018 <- readRDS("data_processed/2018_clean.RDS")
log_new <- readRDS("data_processed/log_new.RDS")
#names(log_new)
# Combine dfs. The older df uses "notes" for the activity name
log_2018_simple <- log_2018 %>% 
  select_at(c(names(log_new)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description)

log_all <- mutate(log_2018_simple, week_data = 0) %>%  
  bind_rows(log_new) %>% 
  mutate(week_data = as.logical(week_data)) %>% 
  rename(name = notes)

count(log_all, week_data)
# names(log_all) <- str_to_lower(names(log_all))

#Other queries
print(log_new, n=15)
totals
log_R
names(log_R)
names(log_new)
log_new %>% group_by(type, week_data) %>% tally(distance)

shoe_summary <- log_all %>% 
  filter(type == "R") %>% 
  group_by(subtype) %>%
  summarise_at(funs(sum), .vars = c("time", "distance", "ascent")) %>%
  arrange(desc(distance))
shoe_summary

#Annual volume
volume <- log_all %>% filter(type %in% c("R", "B")) %>%
  filter(year(date) <= 2019) %>% 
  group_by(type, year = as.factor(year(date))) %>%
  summarise(distance = sum(distance),
            time = sum(time),
            ascent = sum(ascent),
            freq = n())

ggplot(volume, aes(x = year, y = distance, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 60 / 52, fill = type)) +
  geom_col(position = "dodge") +
  ylab("weekly hours")
ggplot(volume, aes(x = year, y = ascent, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = freq, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 60 / 52, fill = type)) +
  geom_col() +
  ylab("weekly hours")

# Five longest (distance) R, B and F activities.
log_all %>% filter(type %in% c("R", "B", "F")) %>%
  filter(week_data == 0) %>%
  select(-week_data) %>% 
  group_by(type) %>%
  top_n(n = 5, wt = distance) %>%
  arrange(type, desc(distance))

log_all %>% filter(type == "B") %>%
  filter(week_data == 0) %>%
  select(-week_data) %>% 
  top_n(n=15, wt=distance) %>%
  arrange(type, desc(distance))

log_all %>% filter(type == "B") %>%
  filter(week_data == 0) %>%
  select(-week_data) %>% 
  top_n(n=15, wt=ascent) %>%
  arrange(type, desc(ascent))

log_all %>% filter(type == "R") %>%
  filter(week_data==0) %>%
  select(-week_data) %>% 
  group_by(year = year(date), long = distance >=20) %>% 
  count()

mostest(log_all, "R", "time")

n_over(log_all, "B", "distance", 60)
n_over(log_all, "B", "time", 120)
n_over(log_all, "R", "ascent", 500)
n_over(log_all, "B", "ascent", 500)


# counts per year by threshold
log_all
# eddington
eddington(log_new, "R")
eddington(log_new, "R", years = c(2013, 2014))
eddington(log_new, "R", years = 2017)
eddington(log_new, "R", "ascent", 20)
eddington(log_new, "B")
eddington(totals, "R", years = 2017)
eddington(log_new, "B", "ascent", 20)
eddington(totals, "R")


library(zoo)
library(tidyverse)
temp <- totals %>% filter(type=="R") %>% transmute(temp=rollsum(distance, 28, alig="right", fill=NA))
plot(temp$temp, typ='l')
plot(tail(temp$temp, 730), typ='l')

count(workouts, type)
count(workouts, quality)

