# Joins customers_gender based on their defaul address zip with ph_zipcodes

# Author: Harvey King
# Version: 2020-07-28

# Libraries
library(tidyverse)

# Parameters
ncr_cities <- c("quezon city", "qc", "makati", "manila", "metro",
                "taguig", "mandaluyong", "pasig", "binondo", "san juan",
                "marikina", "malabon", "paranaque", "parañaque", "caloocan")

ncr_match <- str_c(ncr_cities, collapse = "|")

# Input file
file_customers <- here::here("03 data/customers_gender.rds")
file_zip_ph <- here::here("03 data/zip_ph.rds")

# Output file
file_out <- here::here("03 data/customers_gender_zip.rds")


# ============================================================================
PH_zipcodes <-
  file_zip_ph %>%
  read_rds() %>%
  select(!c(Locale,Area)) %>% # Take out columns that will result in duplicates
  mutate(Zipcode = as.character(Zipcode)) %>%
  distinct()

# Join PH_zipcodes with table to get regional information

file_customers %>%
  read_rds() %>%
  left_join(PH_zipcodes, by = c("da_zip" = "Zipcode")) %>%
  mutate(Region = str_to_title(Region), 
         Macro_Region = str_to_title(Macro_Region)) %>%
  rename(da_region = Region) %>%
  relocate(da_region, .before = da_country) %>%
  rename(da_macro_region = Macro_Region) %>%
  relocate(da_macro_region, .before = da_country) %>%
  mutate(
    
    da_macro_region = if_else(da_macro_region == "Ncr", "NCR", 
                              da_macro_region),
    
    da_macro_region = if_else((is.na(da_macro_region)) & 
                                (str_detect(da_city, ncr_match)), 
                              "NCR", da_macro_region),
    
    da_macro_region = replace_na(da_macro_region,"Unclear"),
    
    da_macro_region = factor(da_macro_region, levels = 
                               c("NCR", "Luzon", "Visayas", 
                                 "Mindanao", "Unclear" )),
    
    da_region =  if_else(da_macro_region == "NCR", "NCR", da_region),
    
    da_region = replace_na(da_region, "Unclear"),
    
    da_region = factor(da_region, levels =
                         
                         c("NCR",
                           "Region 1 - Ilocos Region",
                           "Region 2 - Cagayan Valley",
                           "Region 3 - Central Luzon",
                           "Region 4a - Calabarzon",
                           "Region 4b - Mimaropa",
                           "Region 5 - Bicol Region",
                           "Region 6 - Western Visayas",
                           "Region 7 - Central Visayas",
                           "Region 8 - Eastern Visayas",
                           "Region 9 - Zamboanga Peninsula",
                           "Region 10 - Northern Mindanao",
                           "Region 11 - Davao Region",
                           "Region 12 - Soccsksargen",
                           "Unclear")
                       ),
    da_region = str_remove_all(da_region, "Region.*-") %>% str_to_title()
    ) %>%
  distinct() %>%
  write_rds(file_out)  
