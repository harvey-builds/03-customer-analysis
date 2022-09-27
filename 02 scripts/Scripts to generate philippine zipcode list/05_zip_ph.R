# Reads in manila, luz, vis, min zipcode list to combine them

# Author: Harvey
# Version: 2020-07-26

# Libraries
library(tidyverse)

# Parameters

# Input file
file_man <- here::here("03 data/zip_manila.rds")
file_luz <- here::here("03 data/zip_luzon.rds")
file_vis <- here::here("03 data/zip_visayas.rds")
file_min <- here::here("03 data/zip_mindanao.rds")

# Output file
file_out <- here::here("03 data/zip_ph.rds")

# ============================================================================


read_rds(file_man) %>%
  bind_rows(read_rds(file_luz)) %>%
  bind_rows(read_rds(file_vis)) %>%
  bind_rows(read_rds(file_min)) %>%
  write_rds(file_out)

