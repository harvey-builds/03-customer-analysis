# Reads in orders_gender_zip, to geocode addresses
# Joins orders_gender_zip with geocode, 
# and exports geocode in different formats

# Author: Harvey King
# Version: 2020-07-26

# Libraries
library(tidyverse)
library(sf)
library(mapview)


# Parameters
lat_lim_low = 4.4
lat_lim_high = 21.10
lon_lim_low = 116.40
lon_lim_high = 126.34

# Input file
file_customers <- here::here("03 data/customers_gender_zip.rds")

# Output file
file_geocode_raw <- here::here("03 data/geocode_raw.rds")
file_out <- here::here("03 data/customers_gender_zip_geocode.rds")
file_out_geocode_rds <- here::here("03 data/geocode.rds")
file_out_cust_geo_geojson <- here::here("03 data/cust_geo.geojson")

# ============================================================================

# Read file
customers <-
  file_customers %>%
  read_rds()


# From file, generate clean list of places to geocode
map_places <-
  customers %>%
  select(customer_id,da_address) %>%
  filter(!is.na(da_address)) %>%
  distinct() %>%
  as.data.frame()

# ============================================================================
# Produce geocode list using google maps api

# geocode_list_raw <-
#   map_places %>% 
#   mutate_geocode(da_address)

# Export raw copy of geocode list
# geocode_list_raw %>%
#   as_tibble() %>%
#   write_rds(file_geocode_raw) 

geocode_list_raw <-
  read_rds(file_geocode_raw)


# ============================================================================


# Join orders data with geocoded data and check on Mapview
geocode_list_raw %>%
  filter(!is.na(lon)) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  mapview()


# Replace lat and lon for places beyond the Philippines with NA
geocode_list <-
  geocode_list_raw %>%
  mutate(invalid = case_when(lat < lat_lim_low ~ NA_character_,
                             lat > lat_lim_high ~ NA_character_,
                             lon < lon_lim_low ~ NA_character_,
                             lon > lon_lim_high ~ NA_character_,
                             TRUE ~ "OK" ),
         lat = if_else(is.na(invalid), NA_real_, lat),
         lon = if_else(is.na(invalid), NA_real_, lon)) %>%
  select(!c(invalid,da_address))


# Check on mapview again to see if there are any strays
customers %>%
  left_join(geocode_list, by = c("customer_id" = "customer_id")) %>%
  filter(!is.na(lon)) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    mapview()

         
# Join orders data with geocodes and export as RDS
customers %>%
  left_join(geocode_list, by = c("customer_id" = "customer_id")) %>%
  write_rds(file_out)


# Export geocode list as RDS
geocode_list %>%
  write_rds(file_out_geocode_rds) 


# Join geocode list with orders table and create a geojson for Mapbox!
customers %>%
  left_join(geocode_list, by = c("customer_id" = "customer_id")) %>%
  select(customer_id,lon,lat) %>%
  filter(!is.na(lon)) %>%
  count(lon,lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
  st_write(file_out_cust_geo_geojson, append = FALSE) 

