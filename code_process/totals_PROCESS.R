# Create totals of activities by day for each of R, B, F and save
# Called from MASTER.R

tot_all <- readRDS("data_processed/log_all.RDS") %>%
  total_by_day(ymd("2013-07-01"), today())

if (save_flag){
  saveRDS(tot_all, "data_processed/totals.RDS")
  cli::cli_alert_success("Saved data_processed/totals.RDS")
}
