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
eddington <- function(data, activity_type, measure = "distance", unit_adjust = 1, years = NA) {
  if (!any(is.na(years))){
    data <- filter(data, lubridate::year(date) %in% years)
  }
  vals <- data %>%
    filter(type == activity_type) %>%
    select_at(measure) %>%
    mutate_all(~./unit_adjust) %>%
    unlist(., use.names = F)

  nn <- floor(max(vals))
  n_exceeds <- integer(nn)
  for (i in seq.int(nn)){
    n_exceeds[i] <- sum((vals) >= i)
  }
  edd <- max(which(n_exceeds >= (1:nn)))
  list(eddington = edd, exceeds = n_exceeds)
}

# Returns a tibble of the `n` activities of type `types` with the largest `measure`
# (e.g. 5 runs/cycles with most ascent/distance/time).
# `measure` is unquoted NSE.
mostest <- function(data, measure, types = "rfb" , n = 15) {
  if ("week_data" %in% names(data)){
    data <- filter(data, week_data == 0) %>%
      select(-week_data)
  }
  types <- parse_types(types)
  data %>%
    filter(type %in% types) %>%
    slice_max({{measure}}, n = n) %>%
    arrange(type, desc({{measure}}))
}

#################
# Returns tibble, counts by year, of activities of `type` that have `measure` >= `threshold`.
#################
n_over <- function(data, measure, types = "frb", threshold) {
  types <- parse_types(types)
  data %>%
    filter(type %in% types) %>%
    filter(week_data == 0) %>%
    select(-week_data) %>%
    filter({{measure}} >= threshold) %>%
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
    filter(distance == 0 | (distance >= 0.5 * time))
  if (nrow(errs) == 0) {
    cli::cli_alert_success("No problems found in the data.")
    return(invisible(errs))
  }
  cli::cli_alert_warning("There are problems in the data:")
  return(errs)
}

################
# Create summary tibble daily totals from `log` for each activity type (B, R, F) with
# entries for all dates and types even if there was no activity of that type on that day.
# Also adds an activity count as column `n` (includes week data).
# The range of dates in output runs from `start_date` to `end_date`.
###############
total_by_day <- function(log, start_date, end_date) {
  check_dates(log, start_date, end_date)
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
             fill = list(time = 0, distance = 0, ascent = 0, n = 0)) %>%
    mutate(aer = ifelse(
      type == "R",
      pmin(time / 60, 1),
      pmin(time / 120, 1)
    )) %>%
    mutate(hours = time / 60)
}

# Checks if all dates in `data$date` are between `start_date` and `end_date` (inclusive).
# Errors if not.
check_dates <- function(data, start_date, end_date) {
  if (any(data$date < start_date)){
    stop("There are entries before the start date of ", start_date, call. = FALSE)
  }
  if (any(data$date > end_date)){
    stop("There are entries after the end date of ", end_date, call. = FALSE)
  }
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
    bike == "cube" ~ "a",
    bike == "scottmb" ~ "m"
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

# Read activity log from spreadsheet and process.
# A list is returned (invisibly) with two tibbles: (1) processed, (2) raw.
# If `save_flag = TRUE` the the two sets are also saved.
log_process <- function(year, save_flag = TRUE, data_path = "data") {
  stopifnot(year %in% c("2018", "2020", "2022"))
  if (year == 2018){
    new_names <- c("date", "type", "subtype", "time", "distance", "ascent", "description", "terrain",
                   "total_time", "strides", "sam", "feel", "enjoy", "physical_cost", "mental_cost",
                   "feel_after", "quality", "quality_types", "workout_type", "time_hard", "time_mod",
                   "density", "hilly", "total_work", "time_on", "recovery", "notes")
    cells <- "A:AA"
  }else{
    new_names <-  c("date", "type", "subtype", "time", "distance", "ascent", "ave_power",
                    "norm_power", "description", "terrain", "total_time", "quick", "feel",
                    "enjoy", "physical_cost", "mental_cost", "feel_after", "quality",
                    "quality_types", "time_on", "notes")
    cells <- "A:U"
  }
  if (year == 2022){
    log_master <- googlesheets4::read_sheet("1w9vNdPAB3qxqnqbToHmGBjxSSAN0-ckYuK_96oluSG8",
                                            col_types = "Dccnnnnncnnnnnnnnncnc")
  }else{
    file_name <- file.path(data_path, glue::glue("Activity record {year}.xlsx"))
    log_master <- readxl::read_excel(file_name, sheet = year, range = readxl::cell_cols(cells))
  }
  cli::cli_alert_success("Read activity log from {year}.")

  log <- setNames(log_master, new_names) %>%
    filter(!is.na(type)) %>%
    mutate(date=as.Date(date)) %>%
    log_replace_nas()

  if(save_flag){
    save_name_1 <- glue::glue("data_processed/log_{year}_raw.RDS")
    save_name_2 <- glue::glue("data_processed/log_{year}.RDS")
    saveRDS(log_master, save_name_1)
    saveRDS(log, save_name_2)
    cli::cli_alert_info("Saved {save_name_1} and {save_name_2}.")
  }
  out_list <- list(log, log_master)
  names(out_list) <- paste0(c("log_", "log_raw_"), year)
  invisible(out_list)
}

# Helper function used in `log_process()`.
#Leaves NAs in columns: Description, Quality_types, Notes.
log_replace_nas <- function(x) {
  mutate(x, total_time = ifelse(is.na(total_time), time, total_time)) %>%
    mutate_if(is.numeric, ~replace_na(., 0)) %>%
    mutate_if(is.character, ~replace_na(., ""))
}

# Helper function used in `log_MERGE.R`
log_simplify <- function(log, keep_cols) {
  log %>%
    select(all_of(keep_cols)) %>%
    rename(notes = description) %>%
    mutate(week_data = 0)
}

# Returns a date vector of the last complete week, starting with Monday.
# `today_complete` indicates whether today is counted as complete
# (only matters if today is a Sunday).
last_complete_week <- function(x, today_complete = TRUE) {
  if (today_complete) x <- x + 1
  start <- x - wday(x, week_start = 1) - 6
  return(seq(start, start + 6, by = "day"))
}

# Plot moving averages from totals data.
# A line is given for each exercise type given in `types`. Optionally total lines
# can be added using the `totals` argument, with key names `total_names`.
# `var` is the metric to use for the moving averages (time, distance, ascent, hours, aer, n).
# The year range is inclusive.
# `window` is the number of days to average.
# `totals` is a character vector.
# `types` is a length 1 character vector.
# The elements of `totals` and `types` take the same form giving the types to total or plot
# e.g. "frb" is all three type, "fr" is just foot and running. Torder/case of letters
# does not matter.
plot_ma <- function(dat, var, min_year = -Inf, max_year = Inf, window = 42,
                    totals = NA, totals_names = "All", types = "frb") {
  y_name <- str_to_title(names(select(dat, {{ var }})))
  dat <- rename(dat, metric = {{ var }})
  if (!is.na(totals)){
    stopifnot(length(totals) == length(totals_names))
    totals <- parse_types(totals)
    for (i in seq_along(totals)){
      dat <- bind_rows(dat,
                       ma_totals(dat,
                                 window = window,
                                 total_types = totals[[i]],
                                 total_name = totals_names[[i]]))
    }
  }
  types <- c(parse_types(types), totals_names)
  dat %>%
    filter(type %in% types) %>%
    group_by(type) %>%
    mutate(ma = slide_dbl(metric, mean, .before = window)) %>%
    ungroup() %>%
    filter(between(year(date), min_year, max_year)) %>%
    ggplot(aes(x = date, y = ma, color = type, group = type)) +
    geom_line() +
    ylab(str_to_title(y_name))
}

# Returns a totals data frame with the metric `var` summed over all types given by
# `total_types`.
# `total_types` should be a vector of types
ma_totals <- function(dat, window, total_types, total_name) {
  dat %>%
    filter(type %in% total_types) %>%
    group_by(date) %>%
    summarise(metric = sum(metric), .groups = "drop") %>%
    mutate(type = total_name) %>%
    mutate(ma = slide_dbl(metric, mean, .before = window))
}

# Simpler version of plot_ma() which plots a single line.
# This can be done by plot_ma() but this uses fewer arguments.
# QUESTIONING
# The single moving average sums over types given in `types` (could be a single type).
plot_ma_single <- function(dat, var, min_year = -Inf, max_year = Inf, window = 42,
                           types = "frb") {
  total_name = types
  types <- parse_types(types)
  y_name <- names(select(dat, {{ var }}))
  dat %>%
    rename(metric = {{ var }}) %>%
    ma_totals(window = window, total_types = types, total_name = total_name) %>%
    filter(between(year(date), min_year, max_year)) %>%
    ggplot(aes(x = date, y = ma)) +
    geom_line() +
    ylab(str_to_title(y_name))
}

# Plots a stacked bar chart of totals by time period and type.
# `var` is the metric to use for the moving averages (time, distance, ascent, hours, aer, n).
# The year range is inclusive.
# `window` is the number of days to average.
# `period` is time period to split data into.
# `week_start` is passed to floor_date() if period = "week".
plot_by_period <- function(dat, var, min_year = -Inf, max_year = Inf, window = 42,
                           period = c("week", "month", "year"),
                           week_start = 1) {
  y_name <- names(select(dat, {{ var }}))
  period <- rlang::arg_match(period)
  dat %>%
    rename(metric = {{ var }}) %>%
    mutate(date_unit = floor_date(date, unit = period, week_start = 1)) %>%
    group_by(date_unit, type) %>%
    summarise(metric = sum(metric), .groups = "drop") %>%
    filter(between(year(date_unit), min_year, max_year)) %>%
    ggplot(aes(x = date_unit, y = metric, fill = type)) +
    geom_col() +
    ylab(str_to_title(y_name)) +
    xlab(str_to_title(period))
}

# Exponentially smooth `x` using parameter `k`
# So previous value is weighted with exp(-1/k) and
# current observation with 1 - exp(-1/k)
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

# Chronic training load from a vector `x`
ctl <- function(x) {
  tail(exp_smooth(x, 42), 1)
}

# Acute training load from a vector `x`
atl <- function(x) {
  tail(exp_smooth(x, 7), 1)
}

# Line plot of ATL and CTL for a single metric and totalled over chosen types.
plot_tl <- function(dat, var, min_year = -Inf, max_year = Inf, types = "frb") {
  types <- parse_types(types)
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

# Summarises in a table training loads for all metrics totalled over chosen types.
tl_total <- function(dat,
                     types = "frb",
                     total_name = str_to_upper(types),
                     weekly = TRUE) {
  tbl <- dat %>%
    filter(type %in% str_to_upper(str_split_1(types, ""))) %>%
    group_by(date) %>%
    summarise(across(-type, sum)) %>%
    summarise(across(c(n, hours, distance, ascent, aer), list(ctl = ctl, atl = atl)))
  pivot_longer(tbl, everything(), names_to = "col") %>%
    separate_wider_delim(col, "_", names = c("metric", "load_measure")) %>%
    {if (weekly) mutate(., value = value * 7) else .} %>%
    pivot_wider(id_cols = metric, names_from = load_measure, values_from = value) %>%
    mutate(tsb = (ctl - atl) / ctl) %>%
    mutate(type = total_name, .before = 1)
}

# Summarises in a table training loads for all metrics split by chosen types.
# If `weekly = TRUE` (default) then loads multiplied by 7 (still calculated daily).
tl_type <- function(dat, types = "frb", weekly = TRUE) {
  types <- parse_types(types)
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

# Filter data to dates with year between `min_year` and `max_year` inclusive.
filter_year <- function(data, min_year = -Inf, max_year = Inf) {
  filter(data, between(year(date), min_year, max_year))
}

# Helper to more easily filter data between dates. The dates are given as strings "yyyy-mm-dd".
# If a date is omitted then that end of the range is unlimited.
filter_dates <- function(dat, start_date = NA, end_date = NA) {
  start_date <- if (is.na(start_date)) ymd("1900-01-01") else ymd(start_date)
  end_date <- if (is.na(end_date)) ymd("2100-01-01") else ymd(end_date)
  filter(dat, between(date, start_date, end_date))
}

# Convert "frb" type inputs into c("F", "R", "B").
parse_types <- function(x) {
  str_to_upper(str_split_1(x, ""))
}

# Used in bike_parts_PROCESS.R
# km that tyre has done on front.
# data is event data table for a bike part.
# latest_km is current bike km.
tyre_front_km <- function(dat, latest_km) {
  ev <- dat$event
  if (!("front" %in% ev || "rear" %in% ev)) return(NA) # non-tyre parts
  km <- dat$km
  active = !"retire" %in% ev
  last_km <- if (active) latest_km else km[ev == "retire"]
  if ("to rear" %in% ev) return(km[ev == "to rear"] - km[ev == "front"])
  if (identical("front", ev)) return(latest_km - km[1])
  if ("to front" %in% ev) return(last_km - km[ev == "to front"])
  0
}
