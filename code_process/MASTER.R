# One-stop place from which to run all data processing scripts

# Also used are readxl, glue, cli
library(tidyverse)
library(edwards)
library(lubridate)
source("functions.R")

data_path <- "C:/Users/James/Dropbox/Mine/Personal"
data_path <- "data"

process_folder <- "code_process"
save_flag <- TRUE

if (FALSE){
  source(file.path(process_folder, "log_PROCESS_2013.R"))
  log_process("2018")
  log_process("2020")
}
log_process("2022")
source(file.path(process_folder, "log_MERGE.R"))
check_data(log_all)
source(file.path(process_folder, "totals_PROCESS.R"))
source(file.path(process_folder, "shoes_PROCESS.R"))
source(file.path(process_folder, "bike_parts_PROCESS.R"))
rmarkdown::render("reports/shoes.Rmd")
rmarkdown::render("reports/recent_summary.Rmd")
rmarkdown::render("reports/bike_parts.Rmd")
