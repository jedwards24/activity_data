# Experimenting with rolling averages. 
# Trying tsibble packages.
library(tidyverse)
library(tsibble)
library(lubridate)
theme_set(theme_minimal())
tot <- readRDS("data_processed/totals.RDS")

tot %>% 
  filter(type == "R") %>% 
  select(date, time) %>% 
  mutate(cum = cumsum(time))
  group_by(date) %>% 
  summarise(cum = cumsum(time))

count_nas(tot)
tsb <- tot %>% 
  filter(type %in% c("B", "R", "F")) %>% 
  as_tsibble(index = date, key = type) %>% 
#  arrange(date) %>% 
  mutate(hrs = time / 60)

tsb2 <- tot %>%
  filter(lubridate::year(date) >= 2014) %>% 
  filter(type %in% c("B", "R", "F")) %>% 
  as_tsibble(index = date, key = type) %>% 
  #  arrange(date) %>% 
  mutate(hrs = time / 60 * 7)

tt <- tsb2 %>% 
  group_by_key() %>% 
  mutate(time_ma = slide_dbl(hrs, mean, .size = 42))

tt %>% 
  ggplot(aes(x = date, y = time_ma, color = type)) +
  geom_line()

tt %>% 
  ggplot(aes(x = date, y = time_ma)) +
  geom_line() +
  facet_wrap(~type)

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

# weeks -------
# may be simpler with tile()
twk <- tsb %>% 
  mutate(yrwk = yearweek(date)) %>% 
  filter(type %in% c("B", "R")) %>% 
  filter(lubridate::year(date) >= 2018) %>% 
  nest(data = c(-type, -yrwk))

week_hrs <- twk %>%
  group_by(type) %>% 
  mutate(week_hrs = slide_dbl(data, 
                                ~ sum(.$hrs), .size = 1, .align = "center", .bind = TRUE
  ))

ggplot(week_hrs, aes(x = yrwk, y = week_hrs, fill = type)) +
  geom_col()
week_hrs %>% arrange(desc(week_hrs))

# months ---------------
tmn <- tsb %>% 
  mutate(yrmn = yearmonth(date)) %>% 
  filter(type %in% c("B", "R")) %>% 
#  filter(lubridate::year(date) >= 2018) %>% 
  nest(data = c(-type, -yrmn))

month_hrs <- tmn %>%
  group_by(type) %>% 
  mutate(month_hrs = slide_dbl(data, 
                              ~ sum(.$hrs), .size = 1, .align = "center", .bind = TRUE
  ))

ggplot(month_hrs, aes(x = yrmn, y = month_hrs, fill = type)) +
  geom_col()

# aerobic "points" -------------
# A weighted measure across R,B,F with limit contribution for long activities

# first with totals: 
tsb <- tot %>%
  filter(type %in% c("B", "R", "F")) %>% 
  mutate(aer = ifelse(
    type == "R", 
    time / 60,
    time / 120
  )) %>% 
  group_by(date) %>% 
  summarise(aer = pmin(sum(aer), 2)) %>% 
  as_tsibble(index = date)
  
tt <- tsb %>% 
  mutate(time_ma = slide_dbl(aer, mean, .size = 42))

ggplot(tt, aes(x = date, y = time_ma)) +
  geom_line() +
  geom_hline(yintercept = mean(tt$aer), lty = 2, alpha = 0.5)
mean(tt$aer)

# Load activity data
log_all <- readRDS("data_processed/log_all.RDS")

tsb <- log_all %>%
  filter(type %in% c("B", "R", "F")) %>% 
#  filter(type %in% c("B", "R")) %>% 
  mutate(aer = ifelse(
    type == "R", 
    pmin(time / 60, 2),
    pmin(time / 120, 2)
  )) %>% 
  group_by(date) %>% 
  summarise(aer = sum(aer)) %>% 
  as_tsibble(index = date)  %>% 
  fill_gaps(aer = 0L, .full = TRUE)

tt <- tsb %>% 
  mutate(time_ma = slide_dbl(aer, mean, .size = 42))

ggplot(tt, aes(x = date, y = time_ma)) +
  geom_line()

ggplot(tsb, aes(x = date, y = aer)) +
  geom_line()

# By type
tsb2 <- log_all %>%
  filter(type %in% c("B", "R", "F")) %>% 
  mutate(aer = ifelse(
    type == "R", 
    pmin(time / 60, 1),
    pmin(time / 120, 1)
  )) %>% 
  group_by(date, type) %>% 
  summarise(aer = sum(aer)) %>% 
  as_tsibble(index = date, key = type) %>% 
  fill_gaps(aer = 0L, .full = TRUE) %>% 
  replace_na(list(aer = 0))

tt2 <- tsb2 %>% 
  group_by_key() %>% 
  mutate(aer_ma = slide_dbl(aer, mean, .size = 42))

tt2 %>% 
  filter(year(date) > 2015) %>% 
  ggplot(aes(x = date, y = aer_ma, color = type)) +
  geom_line()

tail(tt)
tail(tt2)
tt2 %>% filter(date > as.Date("2019-12-20"))
tail(tsb)
tail(tsb2)
has_gaps(tsb2)

# aer by year/month & type
tmn2 <- tsb2 %>% 
  mutate(yrmn = yearmonth(date)) %>% 
  filter(type %in% c("B", "R")) %>% 
  #  filter(lubridate::year(date) >= 2018) %>% 
  nest(data = c(-type, -yrmn))

month_aer <- tmn2 %>%
  group_by(type) %>% 
  mutate(month_aer = slide_dbl(data, 
                               ~ mean(.$aer), .size = 1, .align = "center", .bind = TRUE
  ))

ggplot(month_aer, aes(x = yrmn, y = month_aer, fill = type)) +
  geom_col()

