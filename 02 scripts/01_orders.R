# Reads in and writes out orders data

# Author: Harvey King
# Version: 2020-07-22

# Libraries
library(tidyverse)
library(lubridate)

# Parameters

# Input file
file_raw <- here::here("01 data-raw/orders.csv")

# Output file
file_out <- here::here("03 data/orders.rds")


# ============================================================================


file_raw %>%
  read_csv(
    col_types = 
    cols(
      order_cust_id = col_character(),
      c_id = col_character()),
    guess_max = 20000
    ) %>%
# ============================================================================
# Remove test orders, test customers, cancellations and refunds.
# Remove orders where subtotal (order value) was 0.

  filter(str_detect(replace_na(shipping_line,""), 
                    regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(str_detect(replace_na(c_first_name,""), 
                    regex("test", ignore_case = TRUE), negate = TRUE)) %>%
  
  filter(is.na(cancel_reason)) %>%
  filter(replace_na(refund_flag,0) == 0) %>%
  filter(subtotal_price > 0) %>%
  
# ============================================================================
  
transmute(
    # Keys
    order_number,
    customer_id = coalesce(order_cust_id,c_id),
    customer_exists = if_else(is.na(customer_id), "no_id", "has_id"),
    
    # Different Date Values,
    order_datetime = created_at,
    order_year = year(created_at),
    order_date = date(created_at),
    order_closed_date = date(closed_at),
  
    # order_cancelled = if_else( is.na(cancel_reason) | refund_flag == 0, 
    #                            FALSE, TRUE),
    
    # Shipping Line
    
    order_sh_raw = str_to_lower(shipping_line),

    order_sh_type =
      case_when(
        is.na(order_sh_raw) ~ "unknown",
        # str_detect(order_sh_raw,"test") ~ "test",
        str_detect(order_sh_raw,"(pp-)|(pick)") ~ "pickup",
        str_detect(order_sh_raw,"purchase") ~ "purchase",
        TRUE ~ "delivery"),
    
    order_sh_fee = 
      case_when(
        !order_sh_type == "delivery" ~ order_sh_type,
        shipping_paid > 0 ~ "paid",
        shipping_paid == 0 ~ "free",
        TRUE ~ "unknown"),

    order_sh_promo = 
      case_when(
        str_detect(order_sh_raw,"private") ~ "private sale",
        str_detect(order_sh_raw,"k?rastase") ~ "kerastase",
        str_detect(order_sh_raw,"l'or?al") ~ "l'oreal",
        TRUE ~ "none"),
    
    
    order_transac =
      case_when(
        str_detect(order_sh_raw,"(credit)|(pre)|(bank)") ~ "prepaid",
        str_detect(order_sh_raw,"(cod)|(cash)|(upon)") ~ "postpaid",
        order_sh_type == "purchase" ~ "purchase",
        TRUE ~ "unknown"),
    
    
    # Order Sales Metrics
   

    # order_total = order_value + order_sh_value
    order_total = total_price,
    order_value = subtotal_price,
    order_sh_value = shipping_paid,
    
    # order_value = order_fullprice - order_discounts
    order_fullprice = total_line_items_price,
    order_discounts = total_discounts,
    order_items = item_count
    
    ) %>%
  
  write_rds(file_out)
  

  # file %>%
  #   filter(!shipping_line == "test") %>%
  #   filter(is.na(cancel_reason)) %>%
  #   summarise( total = sum(total_price, na.rm = TRUE), 
  #              line_price = sum(total_line_items_price, na.rm = TRUE), 
  #              subtotal = sum(subtotal_price, na.rm = TRUE),
  #              discounts = sum(total_discounts, na.rm = TRUE), 
  #              shipping = sum(shipping_paid, na.rm = TRUE), 
  #              taxes = sum(total_tax, na.rm = TRUE),
  #              refunds = sum(refund_amount, na.rm = TRUE), 
  #              items = sum(item_count, na.rm = TRUE)) %>% View()

  
# ============================================================================



