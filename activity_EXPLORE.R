############
# Explore previously processed and saved RDS files.
############
library(tidyverse)
library(lubridate)
source("activity_FUNC.R")

log18 <- readRDS("data_processed/log_2018.RDS")
log13 <- readRDS("data_processed/log_2013.RDS")

# Combine dfs. The older df uses "notes" for the activity name
log18_simp <- log18 %>% 
  select_at(c(names(log13)[-1], "description")) %>%
  select(-notes) %>% 
  rename(notes = description)

log_all <- mutate(log18_simp, week_data = 0) %>%  
  bind_rows(log13) %>% 
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

#Annual volume
volume <- log_all %>% 
  filter(type %in% c("R", "B")) %>%
  filter(year(date) <= 2019, year(date) > 2013) %>% 
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

# Rides in 2016/17/18 ---------------
log_all %>% 
  mutate(year = year(date)) %>% 
  filter(type == "B",
         week_data == 0,
         year %in% c(2016, 2017, 2018)) %>%
  select(-week_data) %>% 
  group_by(year) %>% 
  top_n(n = 5, wt = distance) %>%
  arrange(year, desc(distance))


# Five longest (distance) R, B and F activities --------------
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

