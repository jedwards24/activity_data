# One-stop place from which to run all data processing scripts

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)
source("functions.R")

data_path <- "C:/Users/James/Dropbox/Mine/Personal"
data_path <- "data"

process_folder <- "code_process"
save_flag <- TRUE

if (FALSE){
  source(file.path(process_folder, "log_PROCESS_2013.R"))
  source(file.path(process_folder, "log_PROCESS_2018.R"))
  source(file.path(process_folder, "log_PROCESS_2020.R"))
}
source(file.path(process_folder, "log_PROCESS_2022.R"))
source(file.path(process_folder, "log_MERGE.R"))
check_data(log_all)
source(file.path(process_folder, "totals_PROCESS.R"))
source(file.path(process_folder, "shoes_PROCESS.R"))
source(file.path(process_folder, "bike_parts_PROCESS.R"))
