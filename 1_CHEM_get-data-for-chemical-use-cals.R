# title: getting data to calculate energy embedded in chemical use
#  this data is used to create Table 3 in the FPP energy calculations
#  it isn't clear how often this table is updated, nor the exact process
#  this code is meant to explore/understand this
# author: "Eric Coronel, edited/compiled by Gina"
# date created: "3/25/2021"
# date updated: 8/8/2022 (Gina)
# notes:


# R Packages And Other Global Options

library(tidyverse)
library(readxl)
library(usdarnass)
library(zoo)

rm(list = ls())

nass_set_key("40DCDA93-B5E8-33FB-B96F-47902CD5E5A0") # for NASS API queries


# get land use from nass --------------------------------------------------

# Batch import for land use data, excluding potatoes
short_desc_land_use <- read_csv("data_references/short_desc_land_use.csv") %>% 
  pull(1)

# short_desc_land_use_excl_potatoes <- short_desc_land_use %>% 
#   filter(str_detect(short_desc, pattern = "POTATOES", negate = T)) %>% # potatoes is a vegetable, not a field crop
#   pull(1)

nass_lu_data_collector <- data.frame() # keep going here, egc - 05/19/2021

for (i in 1:length(short_desc_land_use)) {
  
  tmp1 <- nass_data(source_desc = "SURVEY",
                    sector_desc = "CROPS",
                    #group_desc = "FIELD CROPS",
                    short_desc = short_desc_land_use[i],
                    domain_desc = "TOTAL",
                    agg_level_desc = "NATIONAL",
                    year = ">=1980",
                    reference_period_desc = "YEAR"
  )
  
  nass_lu_data_collector <- rbind(nass_lu_data_collector, tmp1)
  
  print(paste(i, "out of", length(short_desc_land_use)))
  print(short_desc_land_use[i])
  Sys.sleep(0.5)
}

# Batch import for potatoes
# short_desc_potatoes <- short_desc_land_use %>% 
#   filter(str_detect(short_desc, pattern = "POTATOES")) %>% # potatoes is a vegetable, not field crop
#   pull(1)
# 
# data_collector_potatoes <- data.frame()
# 
# for (i in 1:length(short_desc_potatoes)) {
#   
#   tmp1 <- nass_data(source_desc = "SURVEY",
#                     sector_desc = "CROPS",
#                     group_desc = "VEGETABLES",
#                     short_desc = short_desc_potatoes[i],
#                     domain_desc = "TOTAL",
#                     agg_level_desc = "NATIONAL",
#                     year = ">=1980",
#                     reference_period_desc = "YEAR"
#                     )
#   
#   data_collector_potatoes <- rbind(data_collector_potatoes, tmp1)
#   
#   print(paste(i, "out of", length(short_desc_potatoes)))
#   print(short_desc_potatoes[i])
#   Sys.sleep(0.5)
# }

# Batch import for sucrose percent in sugar beets
sugarbeets_sucrose_pct <- nass_data(source_desc = "SURVEY",
                                    sector_desc = "CROPS",
                                    group_desc = "FIELD CROPS",
                                    short_desc = "SUGARBEETS - SUCROSE, MEASURED IN PCT",
                                    domain_desc = "TOTAL",
                                    #agg_level_desc = "NATIONAL",
                                    year = ">=1980",
                                    reference_period_desc = "YEAR"
)

# Binding data
nass_land_use <- 
  nass_lu_data_collector %>% 
  mutate(Value = as.numeric(str_remove_all(string = Value, pattern = ",")),
         year = as.numeric(year)) %>% 
  select(year, commodity_desc, short_desc, Value) %>% 
  rename(Year = year,
         Commodity = commodity_desc,
         `Data Item` = short_desc)

rm(nass_lu_data_collector, short_desc_land_use, tmp1)

nass_land_use %>% write_csv("CHEM_nass-land-use.csv")

# NASS Chemicals with API queries

short_desc_environmental <- 
  read_csv("data_references/short_desc_environmental.csv") %>% 
  pull(1)

# short_desc_environmental_excl_potatoes <- short_desc_environmental %>% 
#   filter(!str_detect(short_desc, pattern = "POTATOES")) %>% 
#   pull(1)

# all crops except potatoes
environ_collector <- data.frame()

for (i in 1:length(short_desc_environmental)) {
  
  tmp1 <- nass_data(source_desc = "SURVEY",
                    sector_desc = "ENVIRONMENTAL",
                    #group_desc = "FIELD CROPS",
                    short_desc = short_desc_environmental[i],
                    agg_level_desc = "REGION : MULTI-STATE",
                    year = ">=1980"
  )
  
  environ_collector <- rbind(environ_collector, tmp1)
  
  print(paste(i, "out of", length(short_desc_environmental)))
  print(short_desc_environmental[i])
  Sys.sleep(0.5)
}

# for potatoes, since it's a vegetable

# short_desc_environmental_potatoes <- short_desc_environmental %>% 
#   filter(str_detect(short_desc, pattern = "POTATOES")) %>% 
#   pull(1)

# environ_potatoes <- nass_data(source_desc = "SURVEY",
#                               sector_desc = "ENVIRONMENTAL",
#                               group_desc = "VEGETABLES",
#                               short_desc = short_desc_environmental_potatoes,
#                               agg_level_desc = "REGION : MULTI-STATE",
#                               year = ">=1980"
#                               )

nass_chemicals <- 
  environ_collector %>%
  #bind_rows(environ_potatoes) %>% 
  select(year, commodity_desc, short_desc, domain_desc, domaincat_desc, Value) %>% 
  rename(Year = year,
         Commodity = commodity_desc,
         `Data Item` = short_desc,
         Domain = domain_desc,
         `Domain Category` = domaincat_desc) %>% 
  mutate(Value = as.numeric(str_remove_all(string = Value, pattern = ",")),
         Year = as.numeric(Year))

nass_chemicals %>% 
  write_csv("CHEM_nass-chemicals.csv")
