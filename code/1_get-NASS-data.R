# purpose Read in NASS data sets needed for chemical calcs
# author: Eric Coronel, Gina
# date created: 3/25/2021
# modified: 8/10/22 - modified to fit needs of metric



library(tidyverse)
library(readxl)
library(usdarnass)

rm(list = ls())


# API setup ---------------------------------------------------------------

#For NASS data, we run API queries
nass_set_key("40DCDA93-B5E8-33FB-B96F-47902CD5E5A0") # for NASS API queries
options(scipen = 999)


# yields, planted area, harvested area ------------------------------------

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


nass_land_use <- 
  nass_lu_data_collector

# Batch import for sucrose percent in sugar beets 
# (the beet people said that's the yield they want to report)
sugarbeets_sucrose_pct <- nass_data(source_desc = "SURVEY",
                                    sector_desc = "CROPS",
                                    group_desc = "FIELD CROPS",
                                    short_desc = "SUGARBEETS - SUCROSE, MEASURED IN PCT",
                                    domain_desc = "TOTAL",
                                    #agg_level_desc = "NATIONAL",
                                    year = ">=1980",
                                    reference_period_desc = "YEAR"
)


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
  environ_collector

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
  apps_collector

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
     nass_chemicals,
     nass_apps,
     sugarbeets_sucrose_pct,
     nass_num_passes,
     file = "data_raw-nass/raw_nass_data.rda")

