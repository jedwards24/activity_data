

library(tidyverse)
library(edwards)
library(lubridate)

dt <- readRDS("data_processed/log_all.RDS") 

bikerun_dates <- c("2013-07-13", "2018-05-26",  "2018-07-03", "2019-02-15", "2019-07-10", "2019-09-25", "2020-08-14",
                   "2020-05-09", "2020-09-11", "2021-05-01", "2021-06-20", "2021-07-14", "2021-07-16", "2021-09-03", 
                   "2021-09-08", "2021-09-15")
dt2 <- dt %>% 
  filter(!week_data) %>% 
  filter(date %in% ymd(bikerun_dates))
count(dt2, date)
dt2
write_csv(dt2, "bikerun.csv")
