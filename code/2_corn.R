# purpose: create data to feed into Energy Table 3 
# author: Gina Nichols
# created: 8/10/22

# NOTE: using Allison's intern's apporach - could be improved


rm(list = ls())

library(tidyverse)
library(readxl)
library(usdarnass)

load("data_raw/nass_data.rda")

rm(corn_pct, nass_land_use, nass_land_use_final, nass_num_passes, sugarbeets_sucrose_pct)

#--load helper functions
source("code/functions/CleanRawChemData.R")
source("code/functions/GetMostRecentTwoYears.R")
source("code/functions/Conversions.R")



# reference table that needs updated --------------------------------------

ref_ai_energy <- 
  read_csv("data_references/audsely2009-table2-ais.csv", skip = 5)

ref_class_energy <- 
  read_csv("data_references/audsley2009-table2-class.csv", skip = 5)


# chem data ---------------------------------------------------------------
nass_apps
nass_chemicals

chem <- CleanRawChemData(f.dapps = nass_apps,
                 f.dchems = nass_chemicals)


my_comm <- "corn"
recent_yrs <- GetMostRecentTwoYears(chem)

# corn herbicides ---------------------------------------------------------

h1 <- 
  chem %>% 
  filter(commodity == my_comm,
         class == "herbicide",
         year %in% recent_yrs) %>% 
  # lumped together the 'glyphosates" and 2-4-d
  mutate(ai2 = 
           case_when(
             grepl("glyphosate", ai) ~ "glyphosate",
             grepl("2,4-d", ai) ~ "2,4-d",
             grepl("dicamba", ai) ~ "dicamba",
             grepl("metolachlor", ai) ~ "metolachlor",
             TRUE ~ ai
           )
  ) %>% 
  group_by(year, commodity, class, ai2) %>% 
  #--sum amounts, but averaged the lbs/ac/app
  summarise(lbsai_ac_app = mean(lbsai_ac_app),
            lbsai = sum(lbsai)) %>% 
  group_by(year, commodity, class) %>% 
  mutate(pct_tot = lbsai/sum(lbsai)) %>% 
  group_by(year, commodity) %>% 
  arrange(year, -pct_tot) %>% 
  group_by(year, commodity) %>% 
  mutate(cum_pct = cumsum(pct_tot)) %>% 
  group_by(year) %>% 
  #take top 4 contributors
  slice_min(cum_pct, n = 4)
  
#--make sure ais are in ref table
h2 <- 
  h1 %>% 
  separate(ai2, into = c("ai", "code"), sep = "=") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  left_join(ref_ai_energy %>% 
              select(ai, intern_MJkg))

h3 <- 
  h2 %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(intern_MJkg),
         btu_ac_app = lbsai_ac_app * energy_btulb) %>% 
  #--now he gets an average weighted by the % it contributes to the total
  group_by(year, commodity, class) %>% 
  summarise(btu_ac_app = weighted.mean(btu_ac_app, pct_tot)) %>% 
  group_by(commodity, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

h_final <- h3

# corn fungicides -------------------------------------------------------

#--note we don't have specific values for each ai, so we use the average (?)

f1 <- 
  chem %>% 
  filter(commodity == my_comm,
         class == "fungicide",
         year %in% recent_yrs) %>% 
  group_by(year, commodity, class) %>% 
  #--sum amounts, but averaged the lbs/ac/app
   summarise(lbsai_ac_app = mean(lbsai_ac_app),
             lbsai = sum(lbsai)) 

#--make sure ais are in ref table
f2 <- 
  f1 %>% 
  left_join(ref_class_energy)

f3 <- 
  f2 %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(energy_MJkgai),
         btu_ac_app = lbsai_ac_app * energy_btulb)  %>% 
  group_by(commodity, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

f_final <- f3


# corn insecticides -------------------------------------------------------

#--note we don't have specific values for each ai, so we use the average (?)

i1 <- 
  chem %>% 
  filter(commodity == my_comm,
         class == "insecticide",
         year %in% recent_yrs) %>% 
  group_by(year, commodity, class) %>% 
  #--sum amounts, but averaged the lbs/ac/app
  summarise(lbsai_ac_app = mean(lbsai_ac_app),
            lbsai = sum(lbsai)) 

#--make sure ais are in ref table
i2 <- 
  i1 %>% 
  left_join(ref_class_energy)

i3 <- 
  i2 %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(energy_MJkgai),
         btu_ac_app = lbsai_ac_app * energy_btulb)  %>% 
  group_by(commodity, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

i_final <- i3

corn_pest_energy <- 
  h_final %>% 
  bind_rows(i_final) %>% 
  bind_rows(f_final)

corn_pest_energy %>% 
  write_csv("data_tidy/pest_energy_corn.csv")
