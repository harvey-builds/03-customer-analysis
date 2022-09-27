
#This script extracts the raw data from google cloud
#Additional set-up. check bigrquery package for necessary steps.


if ("pacman" %in% rownames(installed.packages()) == FALSE) {install.packages("pacman")} # Check if you have universal installer package, install if not
pacman::p_load("tidyverse","partykit","rpart","here","bigrquery") #Check, and if needed install the necessary packages


#CUTSTOMER + ORDERS TABLE
bq_orders <- bq_table(project = "project_name", dataset = "proj", table = "customer_order")
orders <- bq_table_download(bq_orders,max_results = Inf, bigint = c("integer64"))
str(orders)
summary(orders)


#CUSTOMER + LINE ORDERS TABLE
bq_line_orders <- bq_table(project = "project_name", dataset = "proj", table = "customer_line_order")
line_orders <- bq_table_download(bq_line_orders,max_results = Inf, bigint = c("integer64"))
str(line_orders)
summary(line_orders)

#CUSTOMERS TABLE
bq_customers <- bq_table(project = "project_name", dataset = "proj", table = "proj_customers")
customers <- bq_table_download(bq_customers,max_results = Inf, bigint = c("integer64"))
str(customers)


#WRITE OUT
write_csv(orders, here("01 Raw_data","orders.csv"))
write_csv(line_orders, here("01 Raw_data","line_orders.csv"))
write_csv(customers, here("01 Raw_data","customers.csv"))

  