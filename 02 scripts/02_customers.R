# Reads in orders data to generate a customers table

# Author: Harvey King
# Version: 2020-07-22

# Libraries
library(tidyverse)
library(lubridate)

# Parameters

# Input file
file_raw <- here::here("01 data-raw/orders.csv")

# Output file
file_out <- here::here("03 data/customers.rds")


# ============================================================================


file_raw %>%
  read_csv(
    col_types = 
      cols(
        c_id = col_character(),
        c_last_order_id = col_character()
      )
    ,guess_max = 20000
  ) %>%

# ============================================================================    
# Remove test orders, test customers, cancellations and refunds.
# Remove orders where subtotal (order value) was 0.
# Remove orders not associated with a customer. This will be coallesced later.
  
  filter(str_detect(replace_na(shipping_line,""), 
                    regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(str_detect(replace_na(c_first_name,""), 
                    regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(is.na(cancel_reason)) %>%
  filter(replace_na(refund_flag,0) == 0) %>%
  filter(subtotal_price > 0) %>%
  filter(!is.na(c_id)) %>%
  
# ============================================================================     
  transmute(
    
    # Keys
    customer_id = c_id,
    
    customer_datetime = c_created_at,
    customer_date = date(c_created_at),
    customer_lo_date = date(created_at),
    
    # Customer Metrics
    customer_state = c_state,
    customer_verified = if_else(c_verified_email == TRUE, "verified", "not verified"),
    customer_mktg = if_else(c_accepts_mktg == TRUE, "allowed", "not allowed"),
    customer_orders_count = c_orders_count,
    customer_total_value = c_total_spent,
    
    
    # Customer Name
    customer_first_name = str_to_lower(c_first_name) %>% 
      str_replace_all("-"," "),
    customer_last_name = str_to_lower(c_last_name),

    # Default Address Name
    da_name = str_to_lower(da_name),
    da_firstname = str_to_lower(da_firstname) %>%
      str_replace_all("-"," "),
    da_lastname = str_to_lower(da_lastname),
    
    
    # Dummy Names to match gender
    first_name_dummy = 
      case_when(
        str_detect(customer_first_name, "^mr?(a|s)\\.") ~ "Maria",
        str_detect(customer_first_name, "^mr\\.") ~ "Mario",
        str_detect(customer_first_name, "^.{1,5}\\. ") ~ 
          word(customer_first_name, 2, sep = fixed(" ")),
        str_detect(customer_first_name, "^.{1,4}\\.+") ~ 
          word(customer_first_name, 2, sep = fixed(".")),
        TRUE ~ word(customer_first_name, 1, sep = fixed(" "))) %>% str_to_title(),
    
    da_first_name_dummy = 
      case_when(
        str_detect(da_firstname, "^mr?(a|s)\\.") ~ "Maria",
        str_detect(da_firstname, "^mr\\.") ~ "Mario",
        str_detect(da_firstname, "^.{1,5}\\. ") ~ 
          word(da_firstname, 2, sep = fixed(" ")),
        str_detect(da_firstname, "^.{1,4}\\.+") ~ 
          word(da_firstname, 2, sep = fixed(".")),
        TRUE ~ word(da_firstname, 1, sep = fixed(" "))) %>% str_to_title(),

    
    
    # Default Address
    da_address1 = str_to_lower(da_address1) %>% str_trim() %>% str_squish(),
    da_address2 = str_to_lower(da_address2) %>% str_trim() %>% str_squish(),
    da_city = str_to_lower(da_city) %>% str_trim() %>% str_squish(),
    da_province = str_to_lower(da_province) %>% str_trim() %>% str_squish(),
    da_zip = str_extract(da_zip,"[:digit:]{4}") %>% str_trim() %>% str_squish(),
    da_country = "philippines"
    ) %>%
  
  # Combined da_address
  unite("da_address", da_address1:da_country,sep = ", ", 
        na.rm = TRUE, remove = FALSE) %>%
  distinct() %>%
  write_rds(file_out)

# ============================================================================




