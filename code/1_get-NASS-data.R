# purpose Read in NASS data sets needed for chemical calcs
# author: Eric Coronel, Gina
# date created: 3/25/2021
# modified: 8/10/22 - making into R script instead of Rmarkdown

# R Packages And Other Global Options

library(tidyverse)
library(readxl)
library(usdarnass)

rm(list = ls())

nass_set_key("40DCDA93-B5E8-33FB-B96F-47902CD5E5A0") # for NASS API queries
options(scipen = 999)


# yields, planted area, harvested area ------------------------------------

#For NASS data, we run API queries

# has a list of items to call to get yields, planted area, harvested area for our crops

short_desc_land_use <- 
  read_csv("data_references/short_desc_land_use.csv") %>% 
  pull(1)


#--empty data frame to store data
nass_lu_data_collector <- data.frame() 

for (i in 1:length(short_desc_land_use)) {
  
  tmp1 <- nass_data(source_desc = "SURVEY",
                    sector_desc = "CROPS",
                    #group_desc = "FIELD CROPS", #--potato is a vegetable!
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


# Batch import for sucrose percent in sugar beets 
# (they said that's the yield they want to report)
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


# clean land use data here ------------------------------------------------

# simplify
nass_land_use_2 <- 
  nass_land_use %>% 
  rename_all(tolower) %>% 
  select(-commodity) %>% 
  separate(`data item`, c("crop", "data_item"), sep = " - ")

# convert cotton production to lbs (from 480 lb bales)
# convert rice yield to cwt (from lbs)
nass_land_use_3 <- 
  nass_land_use_2 %>% 
  mutate(
    value_2 = case_when(
      crop == "COTTON" & data_item == "PRODUCTION, MEASURED IN 480 LB BALES" ~ value * 480,
      crop == "RICE" & data_item == "YIELD, MEASURED IN LB / ACRE" ~ value / 100, 
      TRUE ~ value
    ),
    data_item_2 = case_when(
      crop == "COTTON" & data_item == "PRODUCTION, MEASURED IN 480 LB BALES" ~ "PRODUCTION, MEASURED IN LB",
      crop == "RICE" & data_item == "YIELD, MEASURED IN LB / ACRE" ~ "YIELD, MEASURED IN CWT / ACRE",
      TRUE ~ data_item
    )
  )

# reshape dataset
nass_land_use_4 <- 
  nass_land_use_3 %>% 
  select(-data_item, -value) %>% 
  rename(data_item = data_item_2,
         value = value_2) %>% 
  separate(data_item, c("data_item_short", "unit"), sep = ", ") %>% 
  mutate(data_item_short = str_to_lower(data_item_short) %>% str_replace_all(" ", "_")) %>% 
  select(-unit) %>% 
  pivot_wider(names_from = data_item_short, 
              values_from = value)

# CORN: estimate acres planted for grain and silage
# based on acres planted for crop overall, by assuming
# equal loss ratio for grain and silage
corn_harvested <- nass_land_use_4 %>% 
  filter(crop %in% c("CORN, GRAIN", "CORN, SILAGE")) %>% 
  separate(crop, c("crop", "grain_or_silage"), sep = ", ") %>% 
  group_by(year, crop) %>% 
  summarise(acres_harvested = sum(acres_harvested))

corn_planted <- nass_land_use_4 %>% 
  filter(crop == "CORN") %>% 
  select(year, crop, acres_planted)

corn <- corn_harvested %>%
  left_join(corn_planted, by = c("year", "crop")) %>% 
  mutate(pct_harvested = acres_harvested / acres_planted) %>% 
  select(year, crop, pct_harvested)

d <- list(
  crop = c("CORN, GRAIN", "CORN, SILAGE"),
  year = seq(1980, 2020, by = 1) # error discovered here, the seq went up to 2018. egc - 04/21/2021
)

d <- cross_df(d) %>% 
  separate(crop, c("crop_main", "grain_or_silage"), sep = ", ", remove = FALSE)

corn_2 <- d %>% 
  left_join(corn, by = c("year", "crop_main" = "crop")) %>% 
  select(year, crop, pct_harvested)

nass_land_use_5 <- nass_land_use_4 %>% 
  left_join(corn_2, by = c("year", "crop")) %>% 
  filter(crop != "CORN") %>% 
  mutate(acres_planted = ifelse(crop %in% c("CORN, GRAIN", "CORN, SILAGE"),
                                acres_harvested / pct_harvested,
                                acres_planted)) %>% 
  select(-pct_harvested) %>% 
  mutate(crop = str_to_sentence(crop))

# ratio of corn grain to corn silage, for use later
corn_pct <- nass_land_use_5 %>% 
  filter(crop %in% c("Corn, grain", "Corn, silage")) %>% 
  select(year, crop, acres_planted) %>% 
  group_by(year) %>% 
  mutate(pct_corn_acres = acres_planted / sum(acres_planted)) %>% 
  select(year, crop, pct_corn_acres)

corn_pct 


# egc 02/24/2021: I am replacing Kathy's sorghum processing with the same processing as corn grain and silage. We then discard sorghum silage since it's not a Field to Market crop

# sorghum: estimate acres planted for grain and silage
# based on acres planted for crop overall, by assuming
# equal loss ratio for grain and silage

sorghum_harvested <- nass_land_use_4 %>% 
  filter(crop %in% c("SORGHUM, GRAIN", "SORGHUM, SILAGE")) %>% 
  separate(crop, c("crop", "grain_or_silage"), sep = ", ") %>% 
  group_by(year, crop) %>% 
  summarise(acres_harvested = sum(acres_harvested))

sorghum_planted <- nass_land_use_4 %>% 
  filter(crop == "SORGHUM") %>% 
  select(year, crop, acres_planted)

sorghum_egc <- sorghum_harvested %>%
  left_join(sorghum_planted, by = c("year", "crop")) %>% 
  mutate(pct_harvested = acres_harvested / acres_planted) %>% 
  select(year, crop, pct_harvested)

d <- list(
  crop = c("SORGHUM, GRAIN", "SORGHUM, SILAGE"),
  year = seq(1980, 2020, by = 1)
)

d <- cross_df(d) %>% 
  separate(crop, c("crop_main", "grain_or_silage"), sep = ", ", remove = FALSE)

sorghum_2_egc <- d %>% 
  left_join(sorghum_egc, by = c("year", "crop_main" = "crop")) %>% 
  select(year, crop, pct_harvested) %>% 
  mutate(crop = str_to_sentence(crop))

nass_land_use_6 <- nass_land_use_5 %>% 
  left_join(sorghum_2_egc, by = c("year", "crop")) %>% 
  filter(crop != "Sorghum") %>% 
  mutate(acres_planted = ifelse(crop %in% c("Sorghum, grain", "Sorghum, silage"),
                                acres_harvested / pct_harvested,
                                acres_planted)) %>% 
  select(-pct_harvested) %>% 
  mutate(crop = str_to_sentence(crop))

# ratio of sorghum grain to sorghum silage, for use later, egc (maybe, let's see)
sorghum_pct <- nass_land_use_6 %>% 
  filter(crop %in% c("Sorghum, grain", "Sorghum, silage")) %>% 
  select(year, crop, acres_planted) %>% 
  group_by(year) %>% 
  mutate(pct_sorghum_acres = acres_planted / sum(acres_planted)) %>% 
  select(year, crop, pct_sorghum_acres)

# Clean sugar beets sucrose content, this will be used when creating output 02
sugarbeets_sucrose_pct_2 <- sugarbeets_sucrose_pct %>% 
  mutate(year = as.numeric(year),
         Value = as.numeric(Value)) %>% 
  group_by(year) %>% 
  summarize(sucrose_pct = round(mean(Value), 1)) %>% 
  mutate(crop = "Sugar beets")

# save final version
nass_land_use_final <- nass_land_use_6 %>% 
  filter(!(crop %in% c("Wheat, spring, (excl durum)", "Wheat, spring, durum", "Wheat, winter",
                       "Sorghum, silage"))) %>% # egc - 03/18 remove crops for which we have no further use
  mutate(crop = ifelse(test = crop == "Sorghum, grain", # rename to be consistent with other datasets
                       yes = "Sorghum",
                       no = crop)) %>% 
  mutate(crop = ifelse(test = crop == "Sugarbeets", # rename to be consistent with other datasets
                       yes = "Sugar beets",
                       no = crop))

nass_land_use_final 

# chemicals amounts (pesticides and fertilizers) -----------------------------

#--list of calls to get applications in lbs for each crop
short_desc_environmental <- 
  read_csv("data_references/short_desc_environmental.csv") %>% 
  pull(1)

#-empty dataframe
environ_collector <- data.frame()


#--read in everything
for (i in 1:length(short_desc_environmental)) {
  
  tmp1 <- nass_data(source_desc = "SURVEY",
                    sector_desc = "ENVIRONMENTAL",
                    #group_desc = "FIELD CROPS", #--bc potatoes are a vegetable
                    short_desc = short_desc_environmental[i],
                    agg_level_desc = "REGION : MULTI-STATE",
                    year = ">=1980"
  )
  
  environ_collector <- rbind(environ_collector, tmp1)
  
  print(paste(i, "out of", length(short_desc_environmental)))
  print(short_desc_environmental[i])
  Sys.sleep(0.5)
}


nass_chemicals <- 
  environ_collector %>%
  select(year, commodity_desc, short_desc, domain_desc, domaincat_desc, Value) %>% 
  rename(Year = year,
         Commodity = commodity_desc,
         `Data Item` = short_desc,
         Domain = domain_desc,
         `Domain Category` = domaincat_desc) %>% 
  mutate(Value = as.numeric(str_remove_all(string = Value, pattern = ",")),
         Year = as.numeric(Year))

# chemical applications (pesticides and fertilizers) -----------------------------

#--list of calls to get applications in lbs/acre/application avg for each crop
short_desc_apps <- 
  read_csv("data_references/short_desc_applications.csv") %>% 
  pull(1)

#-empty dataframe
apps_collector <- data.frame()


#--read in everything
for (i in 1:length(short_desc_apps)) {
  
    tmp1 <- nass_data(source_desc = "SURVEY",
                    sector_desc = "ENVIRONMENTAL",
                    #group_desc = "FIELD CROPS", #--bc potatoes are a vegetable
                    #short_desc = short_desc_apps[i],
                    short_desc = short_desc_apps[i],
                    agg_level_desc = "REGION : MULTI-STATE",
                    year = ">=1980"
  )
  
  apps_collector <- rbind(apps_collector, tmp1)
  
  print(paste(i, "out of", length(short_desc_apps)))
  print(short_desc_apps[i])
  Sys.sleep(0.5)
}


nass_apps <- 
  apps_collector %>%
  select(year, commodity_desc, short_desc, domain_desc, domaincat_desc, Value) %>% 
  rename(Year = year,
         Commodity = commodity_desc,
         `Data Item` = short_desc,
         Domain = domain_desc,
         `Domain Category` = domaincat_desc) %>% 
  mutate(Value = as.numeric(str_remove_all(string = Value, pattern = ",")),
         Year = as.numeric(Year)) %>% 
  as_tibble()




# number of passes for application energy and GHG emissions ---------------

#--says the avg applications
num_passes_vector <- 
  read_csv("data_references/num_passes_short_desc.csv") %>% 
  pull(descriptions)

nass_num_passes <- data.frame()

for (i in 1:length(num_passes_vector)) {
  
  tmp1 <-  nass_data(source_desc = "SURVEY",
                     sector_desc = "ENVIRONMENTAL",
                     #group_desc = "VEGETABLES", # "FIELD CROPS"
                     short_desc = num_passes_vector[i],
                     agg_level_desc = "REGION : MULTI-STATE",
                     year = ">=1980"
  )
  
  nass_num_passes <- rbind(nass_num_passes, tmp1)
  
  print(paste(i, "out of", length(num_passes_vector)))
  print(num_passes_vector[i])
  Sys.sleep(0.5)
}

# save dynamic data outputs -----------------------------------------------


save(nass_land_use,
     corn_pct,
     nass_land_use_final,
     nass_chemicals,
     nass_apps,
     sugarbeets_sucrose_pct,
     nass_num_passes,
     file = "data_raw/nass_data.rda")

