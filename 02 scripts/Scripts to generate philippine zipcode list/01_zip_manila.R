# Reads in raw data and generates rds with relevant zipcodes
# For Metro Manila only

# Author: Harvey King
# Version: 2020-07-26

# Libraries
library(tidyverse)

# Parameters
Macro_Region <- "NCR"
Region <- "NCR"

# Input file
file_raw <- here::here("01 data-raw/zip_manila.csv")
# Output file
file_out <- here::here("03 data/zip_manila.rds")



# ============================================================================

file_raw %>% 
  read_csv(col_names = FALSE, col_types = NULL,
           locale = default_locale(), na = c("", "NA"), quoted_na = TRUE,
           quote = "\"", comment = "", trim_ws = TRUE, skip = 0,
           progress = show_progress(), skip_empty_rows = TRUE) %>%
  
  filter(!is.na(X1)) %>%
  filter(!X1 %in% c("NORTH","SOUTH")) %>%
  mutate(Area = case_when(is.na(X2) ~ X1, TRUE ~ NA_character_)) %>%
  fill(Area) %>%
  filter(!is.na(X2)) %>%
  rename(Locale = X1, Zipcode = X2) %>%
  mutate(Region = Region, Macro_Region = Macro_Region) %>%
  write_rds(file_out)




