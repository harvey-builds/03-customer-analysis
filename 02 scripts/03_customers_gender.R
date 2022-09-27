# Reads in customer table, and generates column with assumed sex based on customer's name

# Author: Harvey
# Version: 2020-07-26

# Libraries
library(tidyverse)
library(gender)

# Parameters

  # Orders data file
file_customers <- here::here("03 data/customers.rds")

  # Output file
file_out <- here::here("03 data/customers_gender.rds")


# ============================================================================

# Create gender list based on order file's unique list

gender_list_1 <- 
  file_customers %>% 
  read_rds() %>% 
  filter(!is.na(first_name_dummy) == TRUE) %>% 
  pull(first_name_dummy) %>% 
  unique() %>% 
  gender(method = "ssa") %>%
  select(name, gender)
  # View()

gender_list_2 <- 
  file_customers %>% 
  read_rds() %>% 
  filter(!is.na(da_first_name_dummy) == TRUE) %>% 
  pull(da_first_name_dummy) %>% 
  unique() %>% 
  gender( method = "ssa") %>%
  select(name, gender)
# View()
    

# Join file and Gender List based on Key
file_customers %>%
  read_rds() %>%
  left_join(gender_list_1, by = c("first_name_dummy" = "name")) %>%
  left_join(gender_list_2, by = c("da_first_name_dummy" = "name")) %>%
  mutate(gender = coalesce(gender.x, gender.y)) %>%
  select(!ends_with(c(".x",".y"))) %>%
  rename(customer_gender = gender) %>%
  relocate(customer_gender, .before = customer_first_name) %>%
  mutate(customer_gender = replace_na(customer_gender, "unclear") %>%
           factor(levels = c("male", "unclear", "female"))) %>%
  # View()
  distinct() %>%
  write_rds(file_out)

