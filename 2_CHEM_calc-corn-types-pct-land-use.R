# purpose: Caluclate percetnage of land in corn silage and grain
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



# load in nass data -------------------------------------------------------

nass_land_use <- read_csv("CHEM_nass-land-use.csv")

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

# simplify
nass_land_use_2 <- 
  nass_land_use %>% 
  rename_all(tolower) %>% 
  select(-commodity) %>% 
  separate(`data item`, c("crop", "data_item"), sep = " - ")

# convert cotton production to lbs (from 480 lb bales)
# convert rice yield to cwt (from lbs)
nass_land_use_3 <- nass_land_use_2 %>% 
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
nass_land_use_4 <- nass_land_use_3 %>% 
  select(-data_item, -value) %>% 
  rename(data_item = data_item_2,
         value = value_2) %>% 
  separate(data_item, c("data_item_short", "unit"), sep = ", ") %>% 
  mutate(data_item_short = str_to_lower(data_item_short) %>% str_replace_all(" ", "_")) %>% 
  select(-unit) %>% 
  spread(data_item_short, value)

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

corn_pct %>% 
  write_csv("CHEM_landuse-corn-types-pct.csv")


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

nass_land_use_final %>% 
  write_csv("CHEM_nass_land_use_final.csv")

