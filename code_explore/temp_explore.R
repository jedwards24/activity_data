# exploring some different columns

library(tidyverse)
library(edwards)

dt <- readRDS("data_processed/log_2018.RDS")

# Quality types -------------
dt2 <- dt %>% 
  filter(quality >=1, type == "R" ) 
count(dt2, workout_type)
count(dt2, quality_types, sort = T)
ty <- str_to_lower(c("F", "S", "A", "V", "H", "M", "U", "D", "R", "T"))
cc <- numeric(length(ty))
for (i in seq_along(ty)) {
  cc[i] <- sum(str_detect(dt2$quality_types, ty[i]), na.rm = T)
}  
names(cc) <- ty
cc
