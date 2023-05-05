# Experimenting with rolling averages.
library(tidyverse)
library(tsibble)
library(lubridate)
library(slider)
theme_set(cowplot::theme_minimal_grid())
tot <- readRDS("data_processed/totals.RDS")

edwards::count_nas(tot)

# Prepare tsibble ------------
# This also adds aerobic "points" which is a weighted measure
# across R,B,F with limit contribution for long activities

# Retain types
tsb <- tot %>%
  mutate(aer = ifelse(
    type == "R",
    pmin(time / 60, 2),
    pmin(time / 120, 2)
  )) %>%
  as_tsibble(index = date, key = type) %>%
  mutate(hrs = time / 60)

# This sums across types
tsb_sum <- tsb %>%
  as_tibble() %>%
  group_by(date) %>%
  summarise(n = sum(n),
            hrs = sum(hrs),
            ascent = sum(ascent),
            distance = sum(distance),
            aer = pmin(sum(aer), 2)) %>%
  as_tsibble(index = date)

tt <- tsb %>%
  filter(year(date) >= 2020) %>%
  mutate(time_ma = slide_dbl(hrs, mean, .before = 42))

tt %>%
  ggplot(aes(x = date, y = time_ma, color = type)) +
  geom_line()

tt %>%
  ggplot(aes(x = date, y = time_ma)) +
  geom_line() +
  facet_wrap(~type)

tt %>%
  as_tibble() %>%
  filter(type == "B") %>%
  slice_max(time_ma, n = 30) %>%
  print(n = Inf)

# weeks -------
week_hrs <- tsb %>%
  filter(lubridate::year(date) >= 2022) %>%
  group_by_key() %>%
  index_by(yrwk = yearweek(date)) %>%
  summarise(week_hrs = sum(hrs), .groups = "drop")

ggplot(week_hrs, aes(x = yrwk, y = week_hrs, fill = type)) +
  geom_col()

# months ---------------
month_hrs <- tsb %>%
  filter(year(date) >= 2018) %>%
  group_by_key() %>%
  index_by(yrmn = yearmonth(date)) %>%
  summarise(month_hrs = sum(hrs), .groups = "drop")

ggplot(month_hrs, aes(x = yrmn, y = month_hrs, fill = type)) +
  geom_col()

# overlay years bike -------------
bike <- tot %>%
  filter(lubridate::year(date) >= 2018) %>%
  mutate(year = factor(year(date))) %>%
  filter(type %in% c("B")) %>%
  as_tsibble(index = date) %>%
  mutate(hrs = time / 60) %>%
  mutate(hrs_ma = slide_dbl(hrs, mean, .before = 42)) %>%
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
rb <- tt %>%
  filter(!is.na(time_ma), type != "F") %>%
  select(type, date, time_ma) %>%
  pivot_wider(names_from = type, values_from = time_ma) %>%
  mutate(year = factor(year(date), ordered = TRUE))

ggplot(rb, aes(x = B, y = R)) +
  geom_point(aes(color = year)) +
  geom_smooth(method = lm, se = FALSE)

cor(rb$R, rb$B)

# aerobic "points" -------------
# A weighted measure across R,B,F with limit contribution for long activities

# first with totals:
aer_ma <- tsb_sum %>%
  filter(year(date) >=2016) %>%
  mutate(aer_ma = slide_dbl(aer, mean, .before = 42))

ggplot(aer_ma, aes(x = date, y = aer_ma)) +
  geom_line() +
  geom_hline(yintercept = mean(tt$aer), lty = 2, alpha = 0.5)

# By type
aer_ma2 <- tsb %>%
  group_by_key() %>%
  mutate(aer_ma = slide_dbl(aer, mean, .before = 42))

aer_ma2 %>%
  filter(year(date) > 2015) %>%
  ggplot(aes(x = date, y = aer_ma, color = type)) +
  geom_line()

# Combined
tsb_sum %>%
  as_tibble() %>%
  mutate(type = "All") %>%
  bind_rows(as_tibble(tsb)) %>%
  filter(year(date) >= 2020) %>%
  group_by(type) %>%
  mutate(aer_ma = slide_dbl(aer, mean, .before = 42)) %>%
  ggplot(aes(x = date, y = aer_ma, group = type, colour = type)) +
  geom_line()

# aer by year/month & type
month_aer <- aer2 %>%
  filter(year(date) >= 2018) %>%
  group_by_key() %>%
  index_by(yrmn = yearmonth(date)) %>%
  summarise(month_aer = sum(aer), .groups = "drop")

ggplot(month_aer, aes(x = yrmn, y = month_aer, fill = type)) +
  geom_col()
