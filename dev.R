library(tidyverse)
library(tsibble)
library(lubridate)
library(slider)
source("functions.R")
theme_set(cowplot::theme_minimal_grid())
tot <- readRDS("data_processed/totals.RDS")


# KJ of work with average power for hour
watts_to_kj <- function(x) {
  3600 * x / 1000
}
watts_to_kj(seq(100, 150, by = 10))
100*3600/1000


# CTL, ATL -----------

# SES
x = 0:30
y = exp(-x/7)
plot(x, y)
exp_smooth <- function(x, k) {
  if (length(x) < 2) return(x)
  aa <- exp(-1/k)
  y <- numeric(length(x))
  y[1] <- x[1]
  for (i in 2:length(x)){
    y[i] <- y[i - 1] * aa + x[i] * (1 - aa)
  }
  y
}
exp(-1/7)
exp_smooth(NA, 7)
exp_smooth(tot$hours, 7)
exp_smooth(rep(.5, 20), 7)
exp_smooth(c(1, rep(0, 8), 2), 7)

tot %>%
  filter(year(date) == 2023) %>%
  group_by(type) %>%
  mutate(ctl_hours = exp_smooth(hours, 42)) %>%
  ggplot(aes(date, ctl_hours, group = type, color = type)) +
  geom_line()
ctl <- function(x) {
  tail(exp_smooth(x, 42), 1)
}
atl <- function(x) {
  tail(exp_smooth(x, 7), 1)
}

# Plot ctl and atl for a single metric summed over given types
plot_tl <- function(dat, var, min_year = -Inf, max_year = Inf, types = "frb") {
  types <- str_to_upper(str_split_1(types, ""))
  y_name <- names(select(dat, {{ var }}))
  dat %>%
    rename(metric = {{ var }}) %>%
    filter(type %in% types) %>%
    group_by(date) %>%
    summarise(metric = sum(metric)) %>%
    mutate(ctl = exp_smooth(metric, 42), atl = exp_smooth(metric, 7)) %>%
    pivot_longer(c(ctl, atl)) %>%
    filter(between(year(date), min_year, max_year)) %>%
    ggplot(aes(x = date, y = value, group = name, color = name)) +
    geom_line() +
    ylab(paste(str_to_title(y_name), "TL")) +
    xlab("Date")
}
plot_tl(tot, hours, 2023, types = "b")
plot_tl(tot, hours, 2023, types = "frb")
plot_tl(tot, distance, 2023, types = "fr")
plot_tl(tot, ascent, 2022, types = "frb")
plot_tl(tot, ascent, 2022, types = "fr")
plot_tl(tot, aer, 2022, types = "frb")

# Current values ------------
tot %>%
  group_by(type) %>%
  summarise(ctl_hrs = ctl(hours),
            atl_hrs = atl(hours))
# by type
x <- tot %>%
  group_by(type) %>%
  summarise(across(c(time, distance, ascent, n, aer, hours), list(ctl = ctl, atl = atl)))
x
pivot_longer(x, -type, names_to = "col") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  separate_wider_delim(col, "_", names = c("metric", "load_measure"))

pivot_longer(x, -type, names_to = "col") %>%
  separate_wider_delim(col, "_", names = c("metric", "load_measure")) %>%
  pivot_wider(id_cols = c(type, metric), names_from = load_measure, values_from = value) %>%
  mutate(tsb = (ctl - atl) / ctl)

# total
x <- tot %>%
  group_by(date) %>%
  summarise(across(-type, sum)) %>%
  summarise(across(c(n, hours, distance, ascent, aer), list(ctl = ctl, atl = atl)))
pivot_longer(x, everything()) %>%
  separate_wider_delim(name, "_", names = c("metric", "load_measure")) %>%
  pivot_wider(id_cols = metric, names_from = load_measure, values_from = value) %>%
  mutate(tsb = (ctl - atl) / ctl)


tl_total <- function(dat, types = "frb", type_name = "All", weekly = TRUE) {
  types <- str_to_upper(str_split_1(types, ""))
  tbl <- dat %>%
    group_by(date) %>%
    summarise(across(-type, sum)) %>%
    summarise(across(c(n, hours, distance, ascent, aer), list(ctl = ctl, atl = atl)))
  pivot_longer(tbl, everything(), names_to = "col") %>%
    separate_wider_delim(col, "_", names = c("metric", "load_measure")) %>%
    {if (weekly) mutate(., value = value * 7) else .} %>%
    pivot_wider(id_cols = metric, names_from = load_measure, values_from = value) %>%
    mutate(tsb = (ctl - atl) / ctl) %>%
    mutate(type = type_name, .before = 1)
}

tl_total(tot, "fr", weekly = F)
tl_total(tot, "r", weekly = F)
tl_total(tot, "fr", weekly = T)

tl_type <- function(dat, types = "frb", weekly = TRUE) {
  types <- str_to_upper(str_split_1(types, ""))
  tbl <- dat %>%
    filter(type %in% types) %>%
    group_by(type) %>%
    summarise(across(c(n, hours, distance, ascent, aer), list(ctl = ctl, atl = atl)))
  pivot_longer(tbl, -type, names_to = "col") %>%
    separate_wider_delim(col, "_", names = c("metric", "load_measure")) %>%
    {if (weekly) mutate(., value = value * 7) else .} %>%
    pivot_wider(id_cols = c(type, metric), names_from = load_measure, values_from = value) %>%
    mutate(tsb = (ctl - atl) / ctl)
}
tl_type(tot, "FR")

x <- tot %>%
  group_by(type) %>%
  summarise(across(c(time, distance, ascent, n, aer, hours), list(ctl = ctl, atl = atl)))
x
pivot_longer(x, -type, names_to = "col") %>%
  pivot_wider(names_from = type, values_from = value) %>%
  separate_wider_delim(col, "_", names = c("metric", "load_measure"))

pivot_longer(x, -type, names_to = "col") %>%
  separate_wider_delim(col, "_", names = c("metric", "load_measure")) %>%
  pivot_wider(id_cols = c(type, metric), names_from = load_measure, values_from = value) %>%
  mutate(tsb = (ctl - atl) / ctl)

# Power plots ------------
log <- readRDS("data_processed/log_2022.RDS") %>%
  mutate(year = factor(year(date)))

log %>%
  filter(year == 2023) %>%
#  filter(type %in% c("F", "R")) %>%
  filter(norm_power > 0) %>%
  ggplot(aes(time, norm_power, colour = type)) +
  geom_point()

log %>%
  #  filter(year(date) == 2022) %>%
  filter(type %in% c("F", "R")) %>%
  filter(norm_power > 0) %>%
  ggplot(aes(time, norm_power, colour = year)) +
  geom_point()

log %>%
  filter(type %in% c("B")) %>%
  filter(norm_power > 0) %>%
  ggplot(aes(time, norm_power, colour = year)) +
  geom_point()

# Frontier -------
log <- readRDS("data_processed/log_2022.RDS")
dtp <- log %>%
  filter(year(date) == 2023) %>%
#  filter(type == "R") %>%
  filter(norm_power > 0) %>%
#  mutate(time = time / 60) %>%
  arrange(desc(time)) %>%
  mutate(cum_max_np = cummax(norm_power)) %>%
  mutate(is_max = (cum_max_np == norm_power))

dtp %>%
  filter(is_max) %>%
  ggplot(aes(x = time, y = cum_max_np, label = description)) +
  geom_point() +
  ggrepel::geom_label_repel(max.overlaps = 20) +
  geom_line(group = 1) +
  theme_minimal() +
  scale_x_log10() +
  labs(x = "Time (mins)", y = "Power (W)")

# table
dtp %>%
  filter(norm_power == cum_max_np) %>%
  select(type, np = norm_power, time, ascent, name = description) %>%
  prinf()

# New AER measure--------------
total_by_day2 <- function(log, start_date, end_date) {
  check_dates(log, start_date, end_date)
  log %>%
    filter(type %in% c("B", "R", "F")) %>%
    mutate(aer = aer_points(type, time, distance, ascent)) %>%
    select(date, type, time, distance, ascent, aer) %>%
    mutate(n = 1) %>%
    group_by(date, type) %>%
    summarise_all(sum) %>%
    ungroup() %>%
    arrange(date) %>%
    complete(date = seq.Date(start_date, end_date, by = "days"),
             type = c("B", "F", "R"),
             fill = list(time = 0, distance = 0, ascent = 0, n = 0, aer = 0)) %>%
    mutate(aer = pmin(aer, 1)) %>%
    relocate(aer, .after = last_col()) %>%
    mutate(hours = time / 60)
}


# Adjusts foot AER by average grade.
# AER is between time / 240 (half bike) if grade is zero to
# time / 120 when grade is `upper_grade`
aer_points <- function(type, time, distance, ascent, upper_grade = 0.5) {
  grade <- pmin(ascent / distance / 100, upper_grade) / upper_grade
  case_when(
    type == "R" ~ pmin(time / 60, 1),
    type == "B" ~ pmin(time / 120, 1),
    TRUE ~ pmin(time / 240 + grade * time / 240 , 1)
  )
}

dt <- readRDS("data_processed/totals.RDS")
summary(dt$aer)


tail(dt)


## Test
log <- readRDS("data_processed/log_all.RDS")
dt1 <- total_by_day(log, ymd("2013-07-01"), max(log$date))
dt2 <- total_by_day2(log, ymd("2013-07-01"), max(log$date))
dt3 <- total_by_day2(log, ymd("2013-07-01"), max(log$date))

identical(dt1, dt2)
tail(dt2)
tail(dt1)
dt1
dt2
waldo::compare(dt1, dt2)
all.equal(dt1, dt2)
tail(dt3)

log2 <- log %>%
  filter(type %in% c("F")) %>%
  mutate(grade = ascent / distance / 100) %>%
  filter(!week_data)

log2 %>%
  ggplot(aes(grade)) +
  geom_histogram()

dt1 %>%
  mutate(aer_new = dt3$aer) %>%
  filter(type %in% c("F")) %>%
  ggplot(aes(aer, aer_new)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, col = 2)
