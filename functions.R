##################
#Input is a data frame. Returns a vector of the minimum of the differences between 
# the dates or "max_week" 
##################
get_week_gaps <- function(log, max_week=7){
  nn <- dim(log)[1]
  diff <- as.numeric(c(log$date[2 : nn], lubridate::today(tzone="UTC")) - log$date)
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
  log_non_week <- log %>% filter(week_total==0)
  log <- log %>% filter(week_total==1)
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
  return(bind_rows(log_non_week, week_df) %>% arrange(date) %>% rename(week_data=week_total))
}

###################
# Moving average for a vector.
# http://www.markhneedham.com/blog/2014/09/13/r-calculating-rolling-or-moving-averages/
# filter() returns a time series object but this just returns the vector part.
###################
move_ave <- function(x,n=7){
  return(as.numeric(stats::filter(x, rep(1/n,n), sides=1)))
}

##################
# Find an Eddington type number. An Eddington number is the number n of rides >=n activities with >=n miles.
# This function is more flexible allowing diffent activity types (argument activity_type is "R", "B" or "F") and other
# measures and units. Give variable for the measure in the "measure" argument and "unit_adjust" to divide
# the measure by.
# By default all dates are included but a integer of vector "years" can be used to filter.
# The number of activities above each integer is returned together with the Eddington number.
##################
eddington <- function(data, activity_type, measure = "Distance", unit_adjust = 1, years = NA) {
  if (!any(is.na(years))){
    data <- filter(data, lubridate::year(Date) %in% years)
  }
  vals <- data %>% filter(type == activity_type) %>% 
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
# Returns a tibble of the `n activities of `activity_type` with the largest "measure" 
# (e.g. 5 runs/cycles with most ascent/distance/time). 
################
mostest <- function(data, activity_type, measure, n = 15) {
  data %>% 
    filter(type == activity_type) %>%
    filter(week_data == 0) %>%
    select(-week_data) %>% 
    top_n(n = n, wt = get(measure)) %>% 
    arrange(type, desc(get(measure)))
}

# Using current tidyeval 
mostest2 <- function(data, activity_type, measure, n = 15) {
  if ("week_data" %in% names(data)){
    data <- filter(data, week_data == 0) %>%
      select(-week_data) 
  }
  data %>% 
    filter(type == activity_type) %>%
    top_n(n = n, wt = .data[[measure]]) %>% 
    arrange(type, desc(.data[[measure]]))
}

#################
# Returns tibble, counts by year, of activities of `type` that have `measure` >= `threshold`.
#################
n_over <- function(data, activity_type, measure, threshold) {
  data %>% 
    filter(type == activity_type) %>%
    filter(week_data == 0) %>%
    select(-week_data) %>% 
    filter(get(measure) >= threshold) %>% 
    group_by(year = lubridate::year(date)) %>%
    count()
}

###############
# Checks supplied data for NAs and for obvious unlikely values.
# Currently these are if distance is zero or greater than time.
# NAs are only reported as a message.
# Any other issues are returned as tibble with just the rows where these
# occur.
################
check_data <- function(dt) {
  if (any(is.na(dt))){
    cli::cli_alert_info("There are NAs in the data.")
  }else{
    cli::cli_alert_success("There no NAs in the data.")
  }
  errs <- dt %>% 
    filter(type %in% c("B", "R", "F")) %>% 
    filter(!week_data) %>% 
    filter(distance == 0 | (distance >= time))
  if (nrow(errs) == 0) {
    cli::cli_alert_success("No problems in the data.")
    return(invisible(errs))
  }
  return(errs)
}

################
# Create summary tibble daily totals from `log` for each activity type (B, R, F) with  
# entries for all dates and types even if there was no activity of that type on that day.
# Also adds an activity count as column `n` (includes week data).
# The range of dates in output runs from `start_date` to `end_date`.
###############
total_by_day <- function(log, start_date, end_date) {
  log %>% 
    filter(type %in% c("B", "R", "F")) %>% 
    select(date, type, time, distance, ascent) %>%
    mutate(n = 1) %>% 
    group_by(date, type) %>% 
    summarise_all(sum) %>%
    ungroup() %>% 
    arrange(date) %>%
    complete(date = seq.Date(start_date, end_date, by = "days"),
             type = c("B", "F", "R"),
             fill = list(time = 0, distance = 0, ascent = 0, n = 0))
}

# Adds rows for "new" and "now" events to a table of bike maintenance `events`.
# `standard_parts` is a vector of parts for which events should be added for all bikes
# named in `bikes`. `new_dates`
expand_events <- function(events, standard_parts, new_dates, bikes = c("cgr", "cube")) {
  parts_all <- expand_grid(bike = bikes, part = standard_parts) %>% 
    bind_rows(select(events_load, bike, part)) %>% 
    distinct() %>% 
    arrange(bike, part)
  
  events_new <- parts_all %>% 
    left_join(new_dates, by = "bike") %>% 
    filter(!part %in% filter(events, event == "new")$part)
  retired <- events %>% 
    filter(event == "retire") 
  
  events_now <- parts_all %>%  
    mutate(date = today() + 1) %>% 
    mutate(event = "now") %>% 
    anti_join(retired, by = c("bike", "part"))
  
  events_new %>%
    bind_rows(events) %>%
    bind_rows(events_now) %>% 
    arrange(bike, part)
}

# Helper function to be used row wise with maintenance event data.
# Returns integer total of distance of activities on `bike` between
# `date1` and `date2`. `data` is standard activity data. 
# `power_data` is activity data filtered to only contain rows where 
# a power meter was used.
part_distance <- function(date1, date2, bike, data, power_data) {
  if (is.na(date1)) date1 <- ymd("1900-01-01")
  if (is.na(date2)) date1 <- ymd("2100-01-01")
  dt_dates <- data %>% 
    filter(date >= date1,
           date < date2)
  subtype_filter <- case_when(
    bike == "cgr" ~ "c",
    bike == "cube" ~ "a"
  )
  if(bike == "4iiii"){
    power <- power_data %>% 
      filter(date >= date1,
             date < date2) %>% 
      pull(distance) %>% 
      sum() 
    return(power)
  }
  filter(dt_dates, subtype == subtype_filter) %>% 
    pull(distance) %>% 
    sum()
}


