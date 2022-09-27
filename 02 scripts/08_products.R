# Reads in line_order data to produce a product table

# Author: Harvey King
# Version: 2020-07-29

# Libraries
library(tidyverse)
library(tidytext)
# library(igraph)
# library(visNetwork)

# Parameters


# Input file
file_line_orders <- here::here("03 data/line_orders.rds")

# Output file
file_out <- here::here("03 data/product.rds")


# ============================================================================

# Extract nouns, adjectives and verbs from product descriptors
# Uncomment during analysis to compress into fewer rows

features <-
  read_rds(file_line_orders) %>%
  unite("features", product_vendor1, product_vendor2, product_type, 
        product_variant_title, product_tags, product_sku, na.rm = TRUE, 
        sep = " ",
        remove = FALSE
  ) %>%
  mutate(features = str_remove_all(features, "\'")) %>%
  mutate(features = str_replace_all(features,"\\W|_"," ") %>% str_squish()) %>%
  mutate(features2 = paste(features, item_predisc_price, sep = " ")) #%>%
  # select(order_number, item_value, features) %>%
  # count(features, name = "item_value", wt = item_value, sort = TRUE)

# ============================================================================  
# PRODUCT BRAND

x1 <- c("american crew" = "american_crew",
        "balmain" = "balmain",
        "biolage" = "biolage", 
        "cnd" = "cnd",
        "cover up" = "cover_up",
        "davines" = "davines",
        "dyson" = "dyson",
        "elgon" = "elgon",
        "gamma piu" = "gamma piu",
        "kerasilk" = "kerasilk",
        "kerastase" = "kerastase",
        "keratin complex" = "keratin_complex",
        "loreal" = "loreal",
        "lakme" = "lakme",
        "mac\\s?pro" = "macpro",
        "nioxin" = "nioxin",
        "olaplex" = "olaplex",
        "revlon" = "revlon",
        "tigi" = "tigi",
        "service|salon|wholesale" = "salon")

x2 <- 
  as.data.frame(x1) %>% 
  rownames_to_column(var = "rules") %>% 
  pull(rules) %>% 
  str_c(collapse = "|") 

x3 <- x1 %>% str_c(collapse = "|")


table_b <-
  features %>%
  mutate(features_mod = str_replace_all(features,x1),
         keyword = str_extract(features, x2),
         brand1 = str_extract(features_mod, x3))

# ============================================================================ 
# Diagnostics for brand1

# table_b %>%
#   count(brand1, wt = item_value, sort = TRUE) %>%
#   mutate(percent = n / sum(n) * 100) %>% View()
# 
# ============================================================================  
# PRODUCT TYPE 1

y1 <- c(
        "kids.*shampoo" = "kids_shampoo",
        "kids.*conditioner" = "kids_conditioner",
        "mens.*grooming|mens.*styling" = "generic_grooming_styling",
        
        "co\\s?wash" = "cowash",
        "shampoo.*liter|liter.*shampoo|shampoo.*large" = "shampoo_large",
        "conditioner.*liter|liter.*conditioner" = "conditioner_large",
        
        # SETS
        
        # Travel Set
        "travel.*(bain|set|kit)" = "travel.set",
        
        # Shampoo and Conditioner Set
        "shampoo.*and.*conditioner" = "shampoo_conditioner.set",
        "kerastase.*special offer.*fluidealiste.*3800" = "shampoo_conditioner.set",
        "kerastase.*permanent bundle.*(3150|3550)" = "shampoo_conditioner.set",
        "loreal.*special offer.*1570" = "shampoo_conditioner.set",
        "elgon.*mixbundle" = "shampoo_conditioner.set",

        
        # Mixed Set
        "loreal.*special offer.*(1870|2720)" = "mixed.set",
        "kerastase.*special offer.*(5100|5400|6750|7150|7450)" = "mixed.set",
        "kerastase.*permanent bundle.*(5130|5400|5700|5850|6075|6165|6435|6500|6705|6750|6795|6850|7550|10305|11450)" = "mixed.set",
        "dav.*bundle" = "mixed.set",
        
     
        # Starter Set
        "nioxin.*starter" = "starter.set",
        "loreal.*special offer.*8098" = "starter.set",
        
        # Cure Treatment Set
        "nioxin.*system kit" = "cure-treatment.set",
        "kerastase.*permanent bundle.*(9945|11050|13005|14450)" = "cure-treatment.set",

        # Cure Treatment
        "kerastase.*cure.*(4150|4450|8600|9300)" = "cure-treatment",
        "kerastase.*special offer.*(4150|4450)" = "cure-treatment",
        "kerastase.*permanent bundle.*(5355|5580)" = "cure-treatment",
        "kerastase.*permanent bundle.*12900" = "cure-treatment",
        "energizing\\s?gel" = "gel-treatment",
        "purifying\\s?gel" = "gel-treatment",
        "nioxin.*treatment" = "cure-treatment",
        
        # Various Treatments
        "bond\\s?builder?|opticare" = "bond_builder-treatment",
        "mask|masque|circle" = "mask-treatment",
        "hair oil\\s.*serum|oil\\s?spray|hair oil\\s(?!shampoo)|hair oil\\s(?!masque)" = "hair_oil",
        "(lipidium|curlbuilding|invisible|reflection|resistance).*serum|kerastase.*special offer.*2950" = "serum",
        "primer|blowdry|heat protector" = "heat_protector",
        
        # Others
        "shampoo" = "shampoo_std",
        "conditioner" = "conditioner_std",
        "colorant|professional hair color| \\d{1,3}$|evo fabuloso|activator|developer|cover up" = "colorant",
        "styling|hairspray" = "styling",
        "tools|grooming|body\\s?wash|dyson" = "grooming_tools",
        "nail\\s?polish" = "nail_polish",
        "service|salon|wholesale" = "services_wholesale",
        "freebie|gift\\s?card" = "free_gift"
        )

y2 <- 
  as.data.frame(y1) %>% 
  rownames_to_column(var = "rules") %>% 
  pull(rules) %>% 
  str_c(collapse = "|") 

y3 <- y1 %>% str_c(collapse = "|")


table_bt <-
  table_b %>%
  mutate(features_mod = str_replace_all(features2,y1),
         keyword = str_extract(features2, y2),
         type1 = str_extract(features_mod, y3)) %>%
  mutate(type1 = if_else(is.na(type1),
                        case_when(str_detect(features,"treatment") ~ "NA-treatment",
                                  str_detect(features,"set|special offer|permanent offer|bundle") ~ "NA.set",
                                  TRUE ~ type1),type1))

# ============================================================================  
# Diagnostics for Type1
table_bt %>%
  count(type1, wt = item_value, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100) #%>% View()

table_bt %>%
  filter(is.na(type1)) %>% #View()
  count(brand1, wt = item_value, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100) #%>% View()
  # filter(str_detect(features, "kerastase")) %>%
  # filter(!str_detect(features, "dav")) %>%
  # select(features, brand1, keyword, type1) %>%
  # distinct() %>% View()


# ============================================================================ 
# CUSTOMER NEED

z1 <- c( 
        # hair type
        "liquid\\s?spell|elasticate|fullyloaded|epicvolume|tigi volume" = "volume_fine_hair",
        "thinning|nioxin|densifique|thickness" = "thinning_hair",
        "nourish|nutri|aura\\s?botanica|ultime|vitamin|mythic\\s?oil|keratin treated" = "nourishing",
        "aging|chrono" = "aging",
        "damage|break|momo|egoboost|recharge|reenergize|resurrection" = "dry_damaged_hair",
        "long hair" = "long_hair",
        "friz|curl|curl enhancing|curly hair|detangle|discipline|fluidealiste|liss|opticare|straighten" = "curly_hair",
        
        # scalp and dandruff
        "dandruf|scalp|specifique|reboot|detoxifyingshampoo" = "dandruff_itchiness",
        "deep cleansing" = "deep_cleansing",
        
        # color
        "silverset|silver|bleach|blond|purple" = "purple_blonde",
        "colorant|hair color| \\d{3}$|color\\s?refresh|faded|cover up|vibrachrom" = "colored_hair",
        "chromatique|color\\s?stay|color\\s?last|color\\s?safe" = "colored_hair",
        "colorcare|developer|activator|colored\\s?hair|evo fabuloso" = "colored_hair",
        
        # specific
        "beach" = "beach",
        "heat|blow\\s?dry|blowdry lotion|beach" = "heat_protection",
        "pregnant" = "pregnant",
        
        # others
        "elnett|stronghairspray|superstar|smalltalk|sugarshock" = "styling_generic",
        "tools|grooming|body\\s?wash|dyson" = "grooming_tools",
        "nail\\s?polish" = "nail_polish",
        "service|salon|wholesale" = "salon",
        "freebie|gift\\s?card" = "free_gift"
       )

z2 <- 
  as.data.frame(z1) %>% 
  rownames_to_column(var = "rules") %>% 
  pull(rules) %>% 
  str_c(collapse = "|") 

z3 <- z1 %>% str_c(collapse = "|")


table_btn <-
  table_bt %>%
  mutate(features_mod = str_replace_all(features,z1),
         keyword = str_extract(features, z2),
         need1 = str_extract(features_mod, z3)) %>%
  mutate(need1 = if_else(is.na(need1),
                       case_when(str_detect(features,"styling") ~ "styling_generic",
                                 str_detect(features,"fine hair") ~ "volume_fine_hair",
                                 TRUE ~ need1),need1))
# ============================================================================ 
# Diagnostics for need1

table_btn %>%
  count(need1, wt = item_value, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

# table_tbn %>%
#   count(brand1, wt = is.na(need1)/n(), sort = TRUE)
# 
# table_tbn %>%
#   group_by(brand1) %>%
#   summarise(x = sum(!is.na(need1))/n()*100) %>% 
#   arrange(desc(x)) %>% View()
# 
# 
# table_btn %>%
#   # filter(is.na(type1)) %>% View()
#   filter(str_detect(features, "sulfate")) %>% 
#   select(features, features_mod, brand1, keyword, type1, need1) %>% 
#   distinct() %>% View()
# 


# ============================================================================ 
# Price Brackets (after renaming columns)

table_btnp <-
  table_btn %>%
  rename(product_type1 = type1, product_brand1 = brand1, product_need1 = need1) %>%
  mutate(product_price_100 = floor(item_predisc_price/100)*100,
         item_value_100 = floor(item_value/100)*100)


  
# ============================================================================ 
# Note Type is still at 18.2% NA


table_btnp <-
  table_btnp %>%
  unite("key1",product_brand1, item_predisc_price, product_need1, remove = FALSE)


key1 <-
  table_btnp %>%
  filter(!is.na(product_type1)) %>%
  filter(!is.na(product_need1)) %>%
  select(key1, product_type1) %>%
  distinct() %>%
  count(key1, sort = TRUE) %>%
  filter(n == 1) %>%
  inner_join(table_btnp,"key1" = "key1") %>%
  select(key1,product_type1) %>%
  filter(!is.na(product_type1)) %>%
  distinct() %>%
  rename(product_type2 = product_type1)


table_btnp2 <-
  table_btnp %>%
  left_join(key1, "key1" = "key1") %>%
  mutate(product_type2 = if_else(is.na(product_type2),
                                 product_type1, product_type2))

table_btnp2 %>%
  count(product_type2, wt = item_value, sort = TRUE) %>%
  mutate(percent = n / sum(n) * 100)

# ============================================================================ 
# Write out file

table_btnp2 %>%
  select(-features2, -features_mod, -keyword, -key1) %>%
  rename(product_features = features, item_predisc_price_100 = product_price_100 ) %>%
  relocate(c(item_discount, item_predisc_price, item_value, item_price_net, item_qty, item_value_100),
           .after = last_col()) %>%
  relocate(item_value_100, .after = item_value) %>%
  relocate(item_predisc_price_100, .after = item_predisc_price) %>%
  relocate(product_type1, .after = product_need1) %>%
  relocate(product_type2, .after = product_type1) %>%
  write_rds(file_out)


# ============================================================================ 






  
