# One-stop place from which to run all data processing scripts 

library(tidyverse)
library(readxl)
library(edwards)
library(lubridate)

process_folder <- "code_process"
save_flag <- TRUE

if (F){
  source(file.path(process_folder, "log_PROCESS_2013.R"))
  source(file.path(process_folder, "log_PROCESS_2018.R"))
}
source(file.path(process_folder, "log_PROCESS_2020.R"))
source(file.path(process_folder, "log_MERGE.R"))
