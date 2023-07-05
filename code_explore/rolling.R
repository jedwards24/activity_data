# Experimenting with rolling averages.
# This is just used for dev now as I have moved the code to plotting functions
library(tidyverse)
library(tsibble)
library(lubridate)
library(slider)
source("functions.R")
theme_set(cowplot::theme_minimal_grid())
tot <- readRDS("data_processed/totals.RDS")

# Plotting function examples ---------
plot_ma(tot, hours, 2020)
plot_ma(tot, ascent, 2020)
plot_ma(tot, distance, 2020)
plot_ma(tot, distance, min_year = 2020, total = "fr", totals_names = "Foot")
plot_ma(tot, hours, min_year = 2020, total = "frb", totals_names = "All")
# the next two are the same
plot_ma(tot, hours, min_year = 2020, total = "frb", totals_names = "All", types = "")
plot_ma_single(tot, hours, min_year = 2020, types = "frb")
plot_ma_single(tot, distance, min_year = 2020, types = "b")
plot_ma_single(tot, hours, min_year = 2017, types = "frb")

plot_ma(tot, aer, 2020, totals = "frb")
plot_by_period(tot, aer, 2016, period = "month")
plot_by_period(tot, distance, 2020, period = "month")
plot_by_period(tot, hours, 2022, period = "week")

# Prepare tsibble ------------

tsb <- tot %>%
  as_tsibble(index = date, key = type)

# weeks -------
# Replaced by plot_by_period() except this uses yearweek() while that uses floor_date()
week_hrs <- tsb %>%
  filter(lubridate::year(date) >= 2022) %>%
  group_by_key() %>%
  index_by(yrwk = yearweek(date)) %>%
  summarise(week_hrs = sum(hours), .groups = "drop")

ggplot(week_hrs, aes(x = yrwk, y = week_hrs, fill = type)) +
  geom_col()

# overlay years bike -------------
bike <- tot %>%
  filter(lubridate::year(date) >= 2018) %>%
  mutate(year = factor(year(date))) %>%
  filter(type %in% c("B")) %>%
  as_tsibble(index = date) %>%
  mutate(hrs_ma = slide_dbl(hours, mean, .before = 42)) %>%
  mutate(dist_ma = slide_dbl(distance, mean, .before = 42)) %>%
  as_tibble() %>%
  mutate(week = as.numeric(date - floor_date(date, unit = "years")) / 7)

bike %>%
  ggplot(aes(x = week, y = hrs_ma, color = year)) +
  geom_line(size = 1)

bike %>%
  ggplot(aes(x = week, y = dist_ma, color = year)) +
  geom_line(size = 1)

ggplot(bike, aes(x = hrs_ma, dist_ma)) +
  geom_point()

# plot B MA against R MA
rb <- tsb %>%
  filter(year(date) >= 2020) %>%
  mutate(time_ma = slide_dbl(hours, mean, .before = 42)) %>%
  filter(!is.na(time_ma), type != "F") %>%
  select(type, date, time_ma) %>%
  pivot_wider(names_from = type, values_from = time_ma) %>%
  mutate(year = factor(year(date), ordered = TRUE))

ggplot(rb, aes(x = B, y = R)) +
  geom_point(aes(color = year)) +
  geom_smooth(method = lm, se = FALSE)

cor(rb$R, rb$B)
