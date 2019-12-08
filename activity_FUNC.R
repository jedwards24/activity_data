##################
#Input is a data frame. Returns a vector of the minimum of the differences between 
# the dates or "max_week" 
##################
get_week_gaps <- function(log, max_week=7){
  nn <- dim(log)[1]
  diff <- as.numeric(c(log$Date[2 : nn], lubridate::today(tzone="UTC")) - log$Date)
  return(pmin(diff, max_week))
}

################
# Splits all week summary data in a activity log over the 
# time following the summary until the next summary or "max_week", whichever comes first.
# The function splits numeric values in columns 5,6,7,9, replicates columns 1,3,4 and 
# increments the date in column 2.
# New week data is recombined with non-week data and sorted by date.
################
split_week_data <- function(log, max_week=7){
  log_non_week <- log %>% filter(Week_total==0)
  log <- log %>% filter(Week_total==1)
  gaps <- get_week_gaps(log, max_week)
  nn <- dim(log)[1]
  locs <- c(1, cumsum(gaps) + 1)
  week_df <- log[1 : sum(gaps), c(1:7, 9)] #create empty df of correct dimensions
  for (i in 1 : nn){
    for (j in 1 : gaps[i]){
      week_df[locs[i] + j - 1, ] <- 
        c(1, log[i, 2] + j - 1, log[i, 3:4], log[i, c(5:7, 9)] / gaps[i])
    }
  }
  return(bind_rows(log_non_week, week_df) %>% arrange(Date) %>% rename(Week_data=Week_total))
}

###################
#Moving average for a vector.
#http://www.markhneedham.com/blog/2014/09/13/r-calculating-rolling-or-moving-averages/
#filter() returns a time series object but this just returns the vector part.
###################
move_ave <- function(x,n=7){
  return(as.numeric(stats::filter(x, rep(1/n,n), sides=1)))
}

##################
# Find an Eddington type number. An Eddington number is the number n of rides >=n activities with >=n miles.
# This function is more flexible allowing diffent activity types (argument type is R, B or F) and other
# measures and units. Give variable for the measure in the "measure" argument and "unit_adjust" to divide
# the measure by.
# By default all dates are included but a integer of vector "years" can be used to filter.
# The number of activities above each integer is returned together with the Eddington number.
##################
eddington <- function(data, type, measure = "Distance", unit_adjust = 1, years = NA) {
  if (!any(is.na(years))){
    data <- filter(data, lubridate::year(Date) %in% years)
  }
  vals <- data %>% filter(Type == type) %>% 
    select_at(measure) %>% 
    mutate_all(~./unit_adjust) %>% 
    unlist(., use.names = F) 
  
  nn <- floor(max(vals))
  n_exceeds <- integer(nn) 
  for (i in 1 : nn){
    n_exceeds[i] <- sum((vals) >= i)
  }
  edd <- max(which(n_exceeds >= (1 : nn)))
  list(eddington = edd, exceeds = n_exceeds)
}

################
# Returns a tibble of the `n activities of `type` with the largest "measure" 
# (e.g. 5 runs/cycles with most ascent/distance/time). 
################
mostest <- function(data, type, measure, n = 15) {
  data %>% 
    filter(Type == type) %>%
    filter(Week_data == 0) %>%
    select(-Week_data) %>% 
    top_n(n = n, wt = get(measure)) %>% 
    arrange(Type, desc(get(measure)))
}

#################
# Returns tibble, counts by year, of activities of `type` that have `measure` >= `threshold`.
#################
n_over <- function(data, type, measure, threshold) {
  data %>% 
    filter(Type == type) %>%
    filter(Week_data == 0) %>%
    select(-Week_data) %>% 
    filter(get(measure) >= threshold) %>% 
    group_by(year = lubridate::year(Date)) %>%
    count()
}

