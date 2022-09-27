# Reads in orders data to create gender list
# Afterwards, joins orders with gender list

# Author: Harvey
# Version: 2020-07-26

# Libraries
library(tidyverse)

# Parameters

  # Orders data file
file_customers <- here::here("03 data/customers_gender_zip_geocode.rds")
file_orders <- here::here("03 data/orders.rds")

  # Output file
file_out <- here::here("03 data/customers_gender_zip_geocode_orders.rds")

# ============================================================================


orders <- 
  file_orders %>% 
  read_rds()
  # View()


# Join orders and customers based on customer_id
file_customers %>%
  read_rds() %>%
  inner_join(orders, by = c("customer_id" = "customer_id")) %>% 
  write_rds(file_out)
  # View()

