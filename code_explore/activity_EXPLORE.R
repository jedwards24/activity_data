# Explore previously processed and saved RDS files.
# See "rolling.R" for rolling average
library(tidyverse)
library(lubridate)
library(reactable)
library(plotly)
library(edwards)
library(tsibble)
library(slider)
source("functions.R")

log_all <- readRDS("data_processed/log_all.RDS")
totals <- readRDS("data_processed/totals.RDS")
parts <- readRDS("data_processed/bike_parts.RDS")

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
  filter(type %in% c("R", "B", "F")) %>%
  filter(year(date) <= 2022, year(date) > 2013) %>%
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
         year %in% c(2013:2022)) %>%
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

# Recent longest rides --------------
weeks <- 20
n <- 20
recent <- log_all %>%
  filter(type == "B") %>%
  mutate(weeks_ago = edwards::diff_days(today(), date) / 7) %>%
  filter(weeks_ago <= weeks) %>%
  arrange(desc(time)) %>%
  top_n(n, time)
recent %>%
  ggplot(aes(weeks_ago, time / 60, label = name)) +
  geom_point(aes(size = ascent)) +
  ggrepel::geom_text_repel() +
  labs(y = "Hours", x = "Weeks Ago")

recent %>%
  ggplot(aes(time / 60, ascent, label = name)) +
  geom_point(aes(size = 1 / sqrt(weeks_ago))) +
  ggrepel::geom_text_repel() +
  labs(x = "Hours", y = "Ascent")

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
mostest(log_all, "R", "distance")
mostest(log_all, "R", "ascent")

mostest(log_all, c("R", "F"), "time", 2023, 2023)

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

# cdf over threshold
dt %>%
  filter(type == "B") %>%
  count(distance) %>%
  arrange(desc(distance)) %>%
  mutate(n_over = cumsum(n)) %>%
  ggplot(aes(distance, n_over)) +
  geom_line()

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

rides %>%
  ggplot(aes(x = days_ago, y = time / 60, size = ascent)) +
  geom_point()

# Strava-style training log ------------
tl <- log_all %>%
  filter(type %in% c("R", "B", "F")) %>%
  filter(year(date) == 2021) %>%
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
  filter(weeks_ago >= 0) %>%
  ggplot(aes(x = wday, y = weeks_ago + pos, size = time, colour = type)) +
  geom_point() +
  scale_size(range = c(10, 30)) +
  ylab("Weeks Ago") +
  xlab("Weekday") +
  ylim(c(NA, 4.5)) +
  guides(size = FALSE) +
  theme_classic()

# recent weeks --------
ww <- log_all %>%
  filter(type %in% c("R", "B", "F")) %>%
  mutate(year_week = yearweek(date)) %>%
  mutate(weeks_ago = yearweek(today()) - year_week) %>%
  filter(weeks_ago <= 12) %>%
  group_by(year_week, type) %>%
  summarise(n = n(),
            dist = sum(distance),
            ascent = sum(ascent),
            time = sum(time) / 60) %>%
  ungroup()

ww %>%
  filter(type != "F") %>%
  ggplot(aes(year_week, time, fill = type)) +
  geom_col()

ww %>%
  ggplot(aes(year_week, time, group = type, colour = type)) +
  geom_line() +
  ylim(c(0, NA))

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

# Consecutive days riding-----------
b2 <- totals %>%
  filter(type %in% c("B", "T")) %>% #just B
  mutate(rides2 = slide_dbl(distance, ~sum(.>0), .before = 1)) %>%
  mutate(dist2 = slide_dbl(distance, sum, .before = 1))

count(b2, type)
count2(b2, rides2)
b2_count <- b2 %>%
  group_by(year = year(date)) %>%
  count2(rides2, sort = F) %>%
  ungroup() %>%
  mutate(across(1:2, as.factor))

b2_count %>%
  ggplot(aes(year, prop, fill = rides2)) +
  geom_col()

# distance total of consecutive rides
b2d <- b2 %>%
  mutate(year = factor(year(date))) %>%
  mutate(across(1:2, as.factor)) %>%
  filter(rides2 == 2)

count2(b2d, year)

b2d %>%
  ggplot(aes(distance, fill = year)) +
  geom_density(alpha = 0.5)

b2d %>%
  ggplot(aes(dist2, year)) +
  geom_boxplot()

# convert to one row per pair of rides

d19 <- b2 %>%
  mutate(r1 = lag(distance),
         r2 = distance) %>%
  filter(year(date) == 2019,
         rides2 != 0) %>%
  mutate(first_of_2 = lead(rides2) == 2) %>%
  filter(first_of_2 | rides2 == 2)
prinf(d19)
d19 %>%
  filter(rides2 == 2) %>%
  select(date, r1, r2) %>%
  mutate(id = row_number()) %>%
  pivot_longer(starts_with("r"), values_to = "dist", names_to = "ride", names_prefix = "r") %>%
  ggplot(aes(id, dist, fill = ride)) +
  geom_col()

# as function
consecutive <- function(totals, n = 2) {
  totals %>%
    filter(type %in% c("B", "T")) %>% #just B atm
    mutate(n_rides2 = slide_dbl(distance, ~sum(.>0), .before = 1)) %>%
    mutate(dist_total = slide_dbl(distance, sum, .before = 1)) %>%
    mutate(dist_1 = lag(distance),
           dist_2 = distance) %>%
    filter(n_rides2 == 2) %>%
    select(date, dist_1, dist_2, dist_total) %>%
    mutate(year = factor(year(date)))
}


cons <- consecutive(totals) %>%
  mutate(id = row_number())
filter(d19, rides2 == 2)
filter(cons, year(date) == 2019)
cons %>%
  select(-dist_total) %>%
  filter(year == 2020) %>%
  pivot_longer(starts_with("dist"), values_to = "dist", names_to = "ride", names_prefix = "dist_") %>%
  ggplot(aes(date, dist, fill = ride)) +
  geom_col()

cons %>% select(-dist_total) %>%
  ggplot(aes(dist_1, dist_2, colour = year)) +
  geom_point()

top5 <- b2d %>%
  group_by(year) %>%
  slice_max(dist2, n = 5) %>%
  mutate(rank = row_number(desc(dist2)))
top5 %>%
  filter(rank == 1)

# Offroad running paces -----------
fr <- log_all %>%
  filter(type == "R") %>%
  mutate(pace = time / distance,
         climb_rate = ascent / distance) %>%
  select(-week_data, -subtype)
fr %>%
  ggplot(aes(pace, climb_rate)) +
  geom_point()
fr %>%
  arrange(desc(pace))
mostest2(fr, "R", "pace")
fr %>%
  arrange(pace)
fr2 <- fr %>%
  filter(pace < 13, climb_rate > 20) %>%
  arrange(desc(climb_rate))
fr2 %>%
  ggplot(aes(pace, climb_rate)) +
  geom_point()
#  geom_smooth(method="lm")
cor(fr2$pace, fr2$climb_rate)
fr2 %>%
  mutate(year = factor(year(date), ordered = TRUE)) %>%
  filter(year > 2015) %>%
  ggplot(aes(pace, climb_rate, colour = year)) +
  geom_point()

fr2 %>%
  arrange(pace)
fr2 %>%
  filter(year(date) == 2021, time > 40)
fr2 %>%
  filter(time > 90, climb_rate > 35, climb_rate < 45)

# bike parts ---------
events_full %>%
  filter(bike == "cube") %>%
  prinf()

events_full %>%
  filter(bike == "scott") %>%
  prinf()
count(events_full, bike)
# shoes------------
shoes <- ungroup(shoes)
count(shoes, id, name) %>% prinf()
count(log_all, subtype)
log_all %>%
  filter(subtype %in% c("", "0")) %>%
  count(subtype, type)

shoes %>%
  filter(year == 2023) %>%
  filter(owned) %>%
  group_by(id, name) %>%
  summarise(dist_run = sum(distance * (type %in% "R")),
            dist_walk = sum(distance * (type %in% "F")),
            dist_all = sum(distance * (type %in% c("F", "R")))) %>%
  arrange(desc(dist_all))
