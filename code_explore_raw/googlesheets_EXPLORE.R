library(tidyverse)
library(edwards)
library(googlesheets4)
library(lubridate)

"https://docs.google.com/spreadsheets/d/1w9vNdPAB3qxqnqbToHmGBjxSSAN0-ckYuK_96oluSG8/"
sh_url <- "https://docs.google.com/spreadsheets/d/1w9vNdPAB3qxqnqbToHmGBjxSSAN0-ckYuK_96oluSG8/edit#gid=1359794142"
id <- as_sheets_id(sh_url)
dr <- googledrive::drive_get("Activity record 2022")
dr$drive_resource[[1]]
dt <- read_sheet("1w9vNdPAB3qxqnqbToHmGBjxSSAN0-ckYuK_96oluSG8",
                 col_types = "Dccnnnnncnnnnnnnnncnc")
?read_sheet
dt
map_int(dt$Subtype, length)
vcount(dt$Subtype)
filter(dt, is.na(Subtype))
