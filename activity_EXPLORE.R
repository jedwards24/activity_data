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
#totals - Daily totals for each of R,B,F (Time, Distance, Ascent)
#
# 2018_clean.RDS is the main 2018 file
log_2018 <- readRDS("data_processed/2018_clean.RDS")
log_new <- readRDS("data_processed/log_new.RDS")
#names(log_new)
log_2018_simple <- log_2018 %>% 
  select_at(c(names(log_new)[-1], "Description")) %>%
  select(-Notes) %>% 
  rename(Notes = Description)

log_all <- mutate(log_2018_simple, Week_data = 0) %>%  
  bind_rows(log_new) %>% 
  mutate(Week_data = as.logical(Week_data)) %>% 
  rename(Sub_type = 'Sub-type') %>% 
  rename(Name = Notes)

count(log_all, Week_data)
# names(log_all) <- str_to_lower(names(log_all))

#Other queries
print(log_new, n=15)
totals
log_R
names(log_R)
names(log_new)
log_new %>% group_by(Type, Week_data) %>% tally(Distance)

shoe_summary <- log_all %>% 
  filter(Type == "R") %>% 
  group_by(Sub_type) %>%
  summarise_at(funs(sum), .vars = c("Time", "Distance", "Ascent")) %>%
  arrange(desc(Distance))
shoe_summary

#Annual volume
volume <- log_all %>% filter(Type %in% c("R", "B")) %>%
  filter(year(Date) <= 2019) %>% 
  group_by(Type, year = as.factor(year(Date))) %>%
  summarise(distance = sum(Distance),
            time = sum(Time),
            ascent = sum(Ascent),
            freq = n())

ggplot(volume, aes(x = year, y = distance, fill = Type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 60 / 52, fill = Type)) +
  geom_col(position = "dodge") +
  ylab("Weekly hours")
ggplot(volume, aes(x = year, y = ascent, fill = Type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = freq, fill = Type)) +
  geom_col(position = "dodge")
ggplot(volume, aes(x = year, y = time / 60 / 52, fill = Type)) +
  geom_col() +
  ylab("Weekly hours")

# Five longest (distance) R, B and F activities.
log_all %>% filter(Type %in% c("R", "B", "F")) %>%
  filter(Week_data == 0) %>%
  select(-Week_data) %>% 
  group_by(Type) %>%
  top_n(n = 5, wt = Distance) %>%
  arrange(Type, desc(Distance))

log_all %>% filter(Type == "B") %>%
  filter(Week_data == 0) %>%
  select(-Week_data) %>% 
  top_n(n=15, wt=Distance) %>%
  arrange(Type, desc(Distance))

log_all %>% filter(Type == "B") %>%
  filter(Week_data == 0) %>%
  select(-Week_data) %>% 
  top_n(n=15, wt=Ascent) %>%
  arrange(Type, desc(Ascent))

log_all %>% filter(Type == "R") %>%
  filter(Week_data==0) %>%
  select(-Week_data) %>% 
  group_by(year = year(Date), long = Distance >=20) %>% 
  count()

mostest(log_all, "R", "Time")

n_over(log_all, "B", "Distance", 60)
n_over(log_all, "B", "Time", 120)
n_over(log_all, "R", "Ascent", 500)
n_over(log_all, "B", "Ascent", 500)


# counts per year by threshold
log_all
# eddington
eddington(log_new, "R")
eddington(log_new, "R", years = c(2013, 2014))
eddington(log_new, "R", years = 2017)
eddington(log_new, "R", "Ascent", 20)
eddington(log_new, "B")
eddington(totals, "R", years = 2017)
eddington(log_new, "B", "Ascent", 20)
eddington(totals, "R")


library(zoo)
library(tidyverse)
temp <- totals %>% filter(Type=="R") %>% transmute(temp=rollsum(Distance, 28, alig="right", fill=NA))
plot(temp$temp, typ='l')
plot(tail(temp$temp, 730), typ='l')

count(workouts, Type)
count(workouts, Quality)

