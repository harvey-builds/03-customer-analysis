Product Analysis using R
================

``` r
# Libraries

# Wrangling
library(tidyverse)
library(lubridate)
library(scales)

# Summary Statistics
library(outliers)

# Text Analysis
library(stringr)        # text cleaning and regular expressions
library(tidytext)       # provides additional text mining functions
library(widyr)

# Clustering
library(factoextra)
library(cluster)

# Assoc Rules
library("arules")
library("arulesViz")

# Plotting
library(ggplot2)
library(ggrepel)
library(gridExtra)
library(GGally)
library(ggridges)
library(kableExtra)
library(wesanderson)

# Mapping
library(leaflet)
library(sf)
library(RColorBrewer)

# Network Visualization
library(igraph)
library(ggraph)
library(visNetwork)
library(data.table)
```

``` r
# Input Path
file_orders <- here::here("03 data/orders.rds")
file_products <- here::here("03 data/product.rds")
file_customers <- here::here("03 data/customers_gender_zip_geocode.rds")
file_co <- here::here("03 data/customers_gender_zip_geocode_orders.rds")
file_zip_ph <- here::here("03 data/zip_ph.rds")

# Read Files
orders.raw <- read_rds(file_orders)
products.raw <- read_rds(file_products)
customers.raw <- read_rds(file_customers)
co.raw <- read_rds(file_co)

# Number Formatting
options(scipen = 999)

# Parameters
# Irrelevant customer_ids
xtest <- c("3273557147747", "3297613348963", "3436247875683", "3210539368547", 
           "4412203910", "2236354953315")

# Table Preferences for Kable
tb.ops <- c("striped", "hover", "condensed", "responsive")
```

``` r
# Customer Product Data 
products.data <- 
  products.raw %>% 
  inner_join(orders.raw, by = c("order_number" = "order_number")) %>%
  filter(order_year >= 2019) %>%
  rename(customer_id = customer_id.x) %>%
  mutate(has_value = if_else(item_value > 0, "has value", "zero value"),
         has_customer = if_else(is.na(customer_id), "no customer id", "has customer id"))
```

### Product Analysis Overview

   
This analysis provides an overview of the website’s product assortment
focusing on specialty hair products.

Products were categorize according to  
1. **price**  
2. **brand**  
3. **primary need** (e.g., coloring, thinning hair, or damaged hair)

**product type** (e.g., shampoo, conditioner, or treatment) could no be
determined for 30% of the orders.  
As a result, it is excluded from the analysis.

### Using text-analysis to group products

The network diagram below shows the most common **product need** and
**product type** for each brand, based on the descriptors used in the
website. Each brand’s respective websites were also consulted to confirm
its positioning.

<div id="htmlwidget-5e7d49363852c572c572" style="width:672px;height:480px;" class="visNetwork html-widget"></div>
<script type="application/json" data-for="htmlwidget-5e7d49363852c572c572">{"x":{"nodes":{"id":["tigi","bed","kerastase","styling","color","sale","professional","revlon","new","loreal","frizz","davines","thinning","sulfate","free","nioxin","mask","shampoo","colored","faded","head","bleached","toning","dry","frizzy","hairloss","system","damaged","volume","conditioner","american","kr","breakage","lp","control","shine","keratin","men","oil","consumer","anti","discipline","salon","curls","leave","davids","featured","arrivals","heat","award","resistance","lakme","biolage","special","refresh","rebonded","cream","k","crew","dandruff","specifique","oily","densifique","fine","protector","hairfall","optimo","grooming","argan","elixir","scalp","repair","purple","serum","density","lockdown","strength","disabled","complex","technical","bundle","curl","in","hairspray","individual","masque","energizing","set","davapr2019pwg","bio","offers","fluidealiste","summer","aging","gel","iron","style","genesis","strong","long","nutritive","150ml","beard","reflection","blowdry","holiday","elgon","oleo","olaplex","sensitive","spotlight"],"label":["tigi","bed","kerastase","styling","color","sale","professional","revlon","new","loreal","frizz","davines","thinning","sulfate","free","nioxin","mask","shampoo","colored","faded","head","bleached","toning","dry","frizzy","hairloss","system","damaged","volume","conditioner","american","kr","breakage","lp","control","shine","keratin","men","oil","consumer","anti","discipline","salon","curls","leave","davids","featured","arrivals","heat","award","resistance","lakme","biolage","special","refresh","rebonded","cream","k","crew","dandruff","specifique","oily","densifique","fine","protector","hairfall","optimo","grooming","argan","elixir","scalp","repair","purple","serum","density","lockdown","strength","disabled","complex","technical","bundle","curl","in","hairspray","individual","masque","energizing","set","davapr2019pwg","bio","offers","fluidealiste","summer","aging","gel","iron","style","genesis","strong","long","nutritive","150ml","beard","reflection","blowdry","holiday","elgon","oleo","olaplex","sensitive","spotlight"],"font.size":[200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200,200],"size":[5,165,5,195,120,95,215,5,145,5,185,5,125,65,115,5,140,255,195,50,165,115,55,195,75,50,40,170,100,175,35,240,95,70,175,105,25,105,100,30,85,35,50,75,85,15,65,145,45,115,25,5,5,90,25,100,35,20,35,15,30,10,25,45,45,30,25,55,10,15,50,20,65,45,30,75,50,15,25,30,15,45,35,25,15,15,10,35,5,25,5,30,35,25,15,15,20,30,15,20,20,25,30,25,10,5,5,15,5,5,15]},"edges":{"from":["tigi","tigi","tigi","kerastase","styling","revlon","loreal","tigi","color","revlon","davines","davines","thinning","tigi","nioxin","tigi","shampoo","colored","nioxin","davines","dry","shampoo","hairloss","davines","tigi","davines","kerastase","davines","tigi","davines","davines","faded","kerastase","kerastase","kerastase","revlon","loreal","shine","tigi","oil","professional","anti","kerastase","kerastase","davines","davines","davines","free","frizz","free","kerastase","kerastase","color","faded","davines","kerastase","control","tigi","loreal","davines","davines","kerastase","kerastase","new","arrivals","tigi","davines","kerastase","davines","kerastase","kerastase","davines","kerastase","davines","styling","kerastase","kerastase","kerastase","davines","davines","tigi","kerastase","professional","lakme","davines","featured","new","arrivals","biolage","davines","davines","kerastase","colored","davines","mask","kerastase","davines","davines","special","kerastase","salon","bleached","davines","kerastase","kerastase","award","davines","nioxin","tigi","tigi","kerastase","styling","lakme","kerastase","davines","davines","kerastase","kerastase","davines","tigi","damaged","thinning","shampoo","kerastase","kerastase","kerastase","kerastase","kerastase","fine","colored","kerastase","tigi","kerastase","kerastase","tigi","davines","frizz","kerastase","kerastase","nioxin","tigi","dry","kerastase","davines","tigi","tigi","tigi","styling","dry","conditioner","kerastase","kerastase","davines","color","bleached","repair","davines","toning","purple","sulfate","kerastase","davines","davines","anti","sulfate","free","color","kerastase","bleached","damaged","revlon","revlon","revlon","davines","shampoo","mask","colored","shampoo","davines"],"to":["sale","bed","head","kr","tigi","professional","lp","lockdown","revlon","salon","mask","damaged","nioxin","professional","system","damaged","tigi","tigi","optimo","professional","tigi","kerastase","nioxin","shampoo","conditioner","color","thinning","free","volume","sulfate","styling","revlon","new","arrivals","damaged","refresh","disabled","tigi","men","lakme","lakme","kerastase","discipline","specifique","salon","consumer","technical","kerastase","tigi","lakme","bundle","frizzy","lakme","lakme","featured","award","tigi","curls","davids","frizz","control","dry","breakage","loreal","loreal","grooming","individual","shine","dry","hairloss","resistance","frizzy","mask","award","loreal","frizz","control","masque","shine","energizing","set","strength","loreal","k","davapr2019pwg","biolage","biolage","biolage","bio","conditioner","volume","fine","lakme","colored","tigi","conditioner","thinning","special","loreal","fluidealiste","lakme","tigi","scalp","scalp","densifique","nioxin","men","men","cream","purple","iron","lakme","style","genesis","dandruff","anti","color","colored","curls","curl","loreal","loreal","lakme","nutritive","oil","elixir","density","volume","nioxin","revlon","special","special","hairfall","leave","leave","curl","loreal","long","in","150ml","hairspray","biolage","serum","rebonded","rebonded","breakage","toning","revlon","revlon","revlon","aging","reflection","leave","loreal","loreal","loreal","holiday","elgon","elgon","kerastase","oleo","oil","bleached","tigi","tigi","tigi","tigi","rebonded","olaplex","olaplex","heat","protector","summer","set","loreal","loreal","loreal","elgon","spotlight"],"n":[186,184,184,122,80,78,70,68,66,52,50,47,44,43,43,41,40,40,35,34,33,32,31,29,28,27,27,26,26,25,25,25,24,24,22,22,22,21,21,21,21,20,20,20,20,20,20,19,19,19,19,18,18,18,17,17,17,17,17,16,16,16,16,16,16,16,16,15,15,15,15,14,14,14,14,13,13,13,13,13,13,13,13,13,13,13,13,13,13,12,12,12,12,11,11,11,11,11,11,11,11,10,10,10,10,10,10,10,10,10,10,10,10,10,9,9,9,9,9,9,9,9,9,9,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,8,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6,6],"length":[10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10]},"nodesToDataframe":true,"edgesToDataframe":true,"options":{"width":"100%","height":"100%","nodes":{"shape":"dot"},"manipulation":{"enabled":false},"physics":{"solver":"forceAtlas2Based","forceAtlas2Based":{"gravitationalConstant":-1000}},"layout":{"randomSeed":123}},"groups":null,"width":null,"height":null,"idselection":{"enabled":false,"style":"width: 150px; height: 26px","useLabels":true,"main":"Select by id"},"byselection":{"enabled":false,"style":"width: 150px; height: 26px","multiple":false,"hideColor":"rgba(200,200,200,0.5)","highlight":false},"main":null,"submain":null,"footer":null,"background":"rgba(0, 0, 0, 0)","highlight":{"enabled":true,"hoverNearest":false,"degree":1,"algorithm":"all","hideColor":"rgba(200,200,200,0.5)","labelOnly":true},"collapse":{"enabled":false,"fit":false,"resetHighlight":true,"clusterOptions":null,"keepCoord":true,"labelSuffix":"(cluster)"}},"evals":[],"jsHooks":[]}</script>

### Pricing distribution

The ridge plots below show how certain product groups are typically
priced higher, or lower than the average. It is also evident some
product groups, such as *colored hair* and *L’oreal* occupy a wide range
of price points.

![](01_Product_files/figure-gfm/products.price.plot-1.png)<!-- -->

### Product positioning

Brands can also be plotted against sales volume and average price to
determine their relative market position. In the chart below, size and
opacity indicate the brand’s share of total company revenue.

![](01_Product_files/figure-gfm/brand.sales.plot-1.png)<!-- -->

Plotting the median line shows each each brand’s impact on the
portfolio. Davines has the highest volume, and sells at the average
price. Kerastase, Nioxin and Olaplex are high-price products, but
Keratase also sells high-volumes. Meanwhile, L’oreal and Tigi sell
similar volumes, but Tigi’s selling price is below average.

![](01_Product_files/figure-gfm/brand.sales.plot2-1.png)<!-- -->

Plotting the brands against product needs shows how some top brands,
such as Nioxin and Olaplex, are highly focused on specific needs, while
Kerastase, davines, L’oreal, and TIGI cater to a wider set of needs.

![](01_Product_files/figure-gfm/brand.need.plot-1.png)<!-- -->
