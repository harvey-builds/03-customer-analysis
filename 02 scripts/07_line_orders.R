# Reads in line_order data to produce a product table

# Author: Harvey King
# Version: 2020-07-29

# Libraries
library(tidyverse)

# Parameters

# Input file
file_raw <- here::here("01 data-raw/line_orders.csv")

# Output file
file_out <- here::here("03 data/line_orders.rds")


# ============================================================================

file_raw %>%
  read_csv(
    col_types = 
      cols(
        order_cust_id = col_character(),
        c_id = col_character(),
        product_id = col_character()),
    guess_max = 20000
  ) %>%
  
# ============================================================================  
# Remove test orders, test customers, cancellations and refunds.
# Note cancellations and returns were extremely low (11 orders)
# Remove ordes where subtotal (order value) was 0.

  filter(str_detect(replace_na(shipping_line,""), 
                  regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(str_detect(replace_na(c_first_name,""), 
                    regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(is.na(cancel_reason)) %>%
  filter(replace_na(refund_flag,0) == 0) %>%
  filter(subtotal_price > 0) %>%
  filter(item_predisc_price > 0) %>% 
  
# ============================================================================ 

  mutate(across(where(is.character),~str_to_lower(.))) %>%
  mutate(across(where(is.character),~str_replace(.,"é","e"))) %>%
  
  transmute(order_number,
            customer_id = c_id,
            product_exists, 
            product_id,
            product_sku = sku,
            product_type = prod_type,
            product_vendor1 = vendor, 
            product_vendor2 = prod_vendor,
            product_variant_title = prod_variant_title,
            product_tags = prod_tags,
            
            item_discount = replace_na(discount_amount,0),
            item_predisc_price = replace_na(item_predisc_price,0),
            item_value = (item_predisc_price * item_qty) - item_discount,
            item_price_net = (item_value / item_qty),
            item_qty) %>%
  
  write_rds(file_out)
  