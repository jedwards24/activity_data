############
# Explore previously processed and saved RDS files.
############
# See "rolling.R" for rolling average 
library(tidyverse)
library(lubridate)
library(reactable)
library(plotly)
library(edwards)
library(tsibble)
source("functions.R")

log_all <- readRDS("data_processed/log_all.RDS")

count(log_all, week_data)
# names(log_all) <- str_to_lower(names(log_all))

#Other queries
print(log_new, n=15)
totals
log_R
names(log_R)
names(log_new)
log_new %>% group_by(type, week_data) %>% tally(distance)

# 2020 month
log_all %>% 
  filter(type %in% c("R", "B")) %>%
  filter(year(date) == 2020) %>% 
  group_by(type, month = factor(month(date), ordered = TRUE)) %>%
  summarise(distance = sum(distance),
            time = sum(time),
            ascent = sum(ascent),
            freq = n())

#Annual volume
volume <- log_all %>% 
  filter(type %in% c("R", "B")) %>%
  filter(year(date) <= 2019, year(date) > 2013) %>% 
  group_by(type, year = as.factor(year(date))) %>%
  summarise(distance = sum(distance),
            time = sum(time) / 60,
            ascent = sum(ascent),
            freq = n())

volume %>% 
  group_by(year) %>% 
  summarise_if(is.numeric, sum)

ggplot(volume, aes(x = year, y = distance, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 52, fill = type)) +
  geom_col(position = "dodge") +
  ylab("weekly hours")
ggplot(volume, aes(x = year, y = ascent, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = freq, fill = type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 52, fill = type)) +
  geom_col() +
  ylab("weekly hours")

# yearly Weekly volume ------------
week_ave <- log_all %>% 
  filter(type %in% c("R", "B")) %>%
  filter(year(date) > 2013) %>%
  group_by(year = as.factor(year(date))) %>%
  mutate(n_weeks = yday(max(date)) / 7) %>% 
  group_by(type, year) %>%
  summarise(n_weeks = max(n_weeks),
            distance = sum(distance) / n_weeks,
            time = sum(time) / 60 / n_weeks,
            ascent = sum(ascent) / n_weeks,
            freq = n() / n_weeks)
week_ave %>% 
  ggplot(aes(x = year, y = time, fill = type)) +
  geom_col(position = "dodge")
week_ave %>%
  ggplot(aes(x = year, y = distance, fill = type)) +
  geom_col(position = "dodge")
week_ave %>%
  ggplot(aes(x = year, y = ascent, fill = type)) +
  geom_col(position = "dodge")

# bike volume ----------
log_all %>%
  filter(type == "B") %>% 
  group_by(subtype) %>% 
  summarise(n = n(), km = sum(distance))
  
log_all %>%
  filter(type == "B", subtype == "(none)") 
    
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

# Rides by distance/pace etc ------------
db <- log_all %>% 
  mutate(year = year(date)) %>% 
  filter(type == "B",
         week_data == 0,
         year %in% c(2013:2020)) %>%
  select(-week_data) %>% 
  mutate(kph_moving = distance / time * 60) %>% 
  mutate(kph_all = distance / total_time * 60) %>%
  mutate(ascent_pc = ascent / distance / 10) %>% 
  mutate(year = factor(year, ordered = T)) %>% 
  mutate_if(is.numeric, ~round(., 1))
  
db %>% 
  filter(distance >= 40) %>% 
  arrange(desc(kph_moving))

db %>% 
  reactable(filterable = T)
gg1 <- db %>% 
  filter(distance >= 60) %>% 
  ggplot(aes(kph_moving, kph_all, colour = year, size = distance, text = name)) +
  geom_point()

gg2 <- db %>% 
  filter(distance >= 60) %>% 
  ggplot(aes(distance, kph_all, colour = year, size = ascent_pc, text = name)) +
  geom_point() 

gg3 <- db %>% 
  filter(distance >= 60) %>% 
  ggplot(aes(distance, kph_moving, colour = year, size = ascent_pc, text = name)) +
  geom_point() 

ggplotly(gg1, tooltip = c("text", "ascent_pc"))
ggplotly(gg2, tooltip = c("text", "ascent_pc"))
ggplotly(gg3, tooltip = c("text", "ascent_pc"))

db %>% 
  filter(distance >= 50) %>% 
  plot_ly(x = ~distance, y = ~kph_all, colour = ~year, size = ~ascent_pc)

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
n_over(log_all, "B", "time", 90)
n_over(log_all, "R", "ascent", 500)
n_over(log_all, "B", "ascent", 500)


# counts per year by threshold
log_all
# eddington
eddington(log_new, "R")
eddington(log_new, "R", years = c(2013, 2014))
eddington(log_new, "R", years = 2017)
eddington(log_all, "B", "ascent", 20)
eddington(log_all, "B", "distance")
eddington(totals, "R", years = 2017)
eddington(log_new, "B", "ascent", 20)
eddington(totals, "R")

count(workouts, type)
count(workouts, quality)

# weekly totals -----------
dfw <- log_all %>% 
  filter(type %in% c("R", "B", "F")) %>% 
  group_by(yearweek = yearweek(date), type) %>% 
  summarise(n = n(), 
            time = sum(time),
            distance = sum(distance),
            ascent = sum(ascent)) %>% 
  ungroup()
dfw %>% 
  filter(type == "B") %>% 
  top_n(n = 15, wt = ascent) %>% 
  arrange(desc(ascent))

dfw %>% 
  filter(type == "B") %>% 
  top_n(n = 15, wt = distance) %>% 
  arrange(desc(distance))

n_over(dfw, "B", "distance", 100)


# 2020 -----------
dt20 <- readRDS("data_processed/log_2020.RDS") 
var_summary(dt20)
dt20 %>% 
  filter(type == "B") %>% 
  group_by(month = month(date)) %>% 
  summarise(n = n(), km = sum(distance), climb = sum(ascent), time = sum(time))

rides <- dt20 %>% 
  filter(type == "B") %>% 
  mutate(days_ago = (today() - date) %>% as.numeric(., units = "days")) %>% 
  select(days_ago, time, distance, ascent, norm_power, description) %>% 
  arrange(days_ago)
rides %>% view
rides %>% 
  ggplot(aes(x = days_ago, y = distance, size = ascent)) +
  geom_point()

rides %>% 
  ggplot(aes(x = days_ago, y = distance, fill = ascent)) +
  geom_col() +
  scale_fill_gradient2()

rides %>% 
  ggplot(aes(size = sqrt(1 / days_ago), x = distance, y = ascent)) +
  geom_point()

# Strava-style training log ------------
tl <- log_all %>% 
  filter(type %in% c("R", "B", "F")) %>% 
  filter(year(date) == 2020) %>% 
  mutate(weeks_ago = isoweek(today()) - isoweek(date),
         wday = wday(date, label = TRUE)) %>% 
  filter(weeks_ago <= 4) %>%
  group_by(date) %>% 
  mutate(act_ind = row_number(time),
         n_acts = n(),
         pos = act_ind / (n_acts + 1) - 0.5) %>% 
  arrange(weeks_ago)
tl
isoweek(tl$date[1])
(1:3) / 4
tl %>% 
  filter(any(act_ind > 1))
tl %>% 
  ggplot(aes(x = wday, y = weeks_ago + pos, size = distance, colour = type)) +
  geom_point() + 
  scale_size(range = c(10, 30)) +
  ylab("Weeks Ago") +
  xlab("Weekday") +
  ylim(c(NA, 4.5)) +
  guides(size = FALSE) +
  theme_classic()

# recent weeks --------
ww <- log_all %>% filter(type %in% c("R", "B", "F")) %>% 
  filter(year(date) == 2020) %>% 
  mutate(week = isoweek(date)) %>% 
  mutate(weeks_ago = isoweek(today()) - week) %>% 
#  filter(weeks_ago <= 4) %>%
  group_by(week, type) %>% 
  summarise(n = n(),
            dist = sum(distance),
            ascent = sum(ascent), 
            time = sum(time) / 60)

ww %>% 
  filter(type != "F") %>% 
  ggplot(aes(week, time, fill = type)) +
  geom_col()

ww %>%  
  ggplot(aes(week, time, group = type, colour = type)) +
  geom_line() +
  ylim(c(0, NA))

view(ww)
# bike rides -----------
# longest ride each week
rides <- log_all %>% 
  group_by(week = floor_date(date, unit = "weeks")) %>%
  filter(!week_data,
         type == "B") %>% 
  mutate(rank = row_number(desc(distance))) %>% 
  filter(rank == 1)

rides %>% 
  filter(year(date) > 2018) %>% 
  ggplot(aes(week, distance)) +
  geom_point()

brmonth <- log_all %>% 
  filter(!week_data,
         type == "B") %>% 
  mutate(dist = bin_integer(distance, c(40, 60, 80))) %>% 
  mutate(month = floor_date(date, unit = "months")) 
  count(month, dist)
  
brmonth %>% 
  filter(dist != "40 or less") %>% 
  ggplot(aes(month, fill = dist)) +
  geom_bar()

rides %>% 
  filter(year(date) > 2018,
         distance > 40)

brmonth  %>% 
  filter(year(date) > 2018,
         distance > 40) %>% 
  ggplot(aes(month, ascent, size = distance)) +
  geom_point()


log_all %>% 
  filter(type == "B", !week_data) %>% 
  ggplot(aes(date, distance, size = distance, colour = distance)) +
  geom_point()

