# purpose Clean chemical data, taken from NIR cleaning porcess, needs to be adjusted for metrics purposes
# author: Eric Coronel, Gina
# date created: 3/25/2021
# modified: 8/10/22 - making into R script instead of Rmarkdown

# NOTE: I emailed Allison and the intern's apporach was different from Erics. 
# I will try to replicate the intern's approach here

library(tidyverse)
library(readxl)
library(usdarnass)

load("data_raw/nass_data.rda")

CleanData <- function(d){
  d %>% 
    janitor::clean_names() %>% 
    mutate_if(is.character, str_to_lower)
  
}

ConvMJkgToBTUlb <- function(x){
  x * 947 /2.2
}

# ref tables --------------------------------------------------------------

ref_ai_energy <- 
  read_csv("data_references/audsely2009-table2-ais.csv", skip = 5)


# chem data ---------------------------------------------------------------


#--remove fertilizer applications, keep only data post-2000 (not actually sure)

chem_app <- 
  nass_apps %>% 
  filter(Domain != "FERTILIZER") %>% 
  filter(Year > 2000) %>% 
  CleanData(.) %>% 
  rename("lbsai_ac_app" = value) %>% 
  select(-data_item, -domain)

chem_tot <- 
  nass_chemicals %>% 
  filter(Domain != "FERTILIZER") %>% 
  filter(Year > 2000) %>% 
  as_tibble() %>% 
  CleanData(.) %>% 
  rename("lbsai" = value) %>% 
  select(-data_item, -domain)


chem <- 
  chem_app %>% 
  left_join(chem_tot) %>% 
  filter(!is.na(lbsai_ac_app)) %>% 
  mutate(domain_category = str_remove_all(domain_category, "\\(|\\)"),
         domain_category = str_remove_all(domain_category, "chemical, ")) %>% 
  separate(domain_category, into = c("class", "ai"), sep = ":") %>% 
  mutate_if(is.character, str_trim, "both") 
  
  

# intern replications -----------------------------------------------------

#--start with corn

#--intern got 2014 and 2016 for corn  
#--note he did not reassign classes
# he lumped together the 'glyphosates"


#--what are the most recent years of data for corn?

chem %>% 
  ungroup() %>% 
  filter(commodity == "corn",
         class == "herbicide") %>% 
  select(year) %>% 
  distinct() %>% 
  arrange(-year) %>% 
  slice(1:2) %>% 
  pull(year)

# corn 2016 herb ----------------------------------------------------------


corn16H <- 
  chem %>% 
  filter(commodity == "corn",
         year ==2016,
         class == "herbicide") %>% 
  mutate(ai2 = ifelse(grepl("glyphosate", ai), "glyphosate", ai)) %>% 
  group_by(year, commodity, class, ai2) %>% 
  #--intern summed theamounts, but averaged the lbs/ac/app
  summarise(lbsai_ac_app = mean(lbsai_ac_app),
            lbsai = sum(lbsai),
            pct_tot = sum(pct_tot)) %>% 
  arrange(-pct_tot) %>% 
  group_by(year, commodity, class) %>% 
  mutate(pct_tot = lbsai/sum(lbsai),
         cum_pct = cumsum(pct_tot)) %>% 
  #--he says 'select products that account for 80% or more of total amount applied'
  #--this is subjective - he chose 92% as his cutoff
  filter(cum_pct < 0.92) %>% 
#  select(year, commodity, class, ai2, lbsai_ac_app) %>% 
  # now he picks out the ais from table 2 and puts them in here
  separate(ai2, into = c("ai", "code"), sep = "=") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  left_join(ref_ai_energy %>% 
              select(ai, intern_MJkg)) %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(intern_MJkg),
         btu_ac_app = lbsai_ac_app * energy_btulb) %>% 
  #--now he gets an average weighted by the % it contributes to the total
  group_by(year, commodity, class) %>% 
  summarise(btu_ac_app = weighted.mean(btu_ac_app, pct_tot))

#--the value is off because of rounding, I believe
corn16H


# corn 2014 herb ----------------------------------------------------------

corn14H <- 
  chem %>% 
  filter(commodity == "corn",
         year ==2014,
         class == "herbicide") %>% 
  mutate(ai2 = ifelse(grepl("glyphosate", ai), "glyphosate", ai)) %>% 
  group_by(year, commodity, class, ai2) %>% 
  #--intern summed theamounts, but averaged the lbs/ac/app
  summarise(lbsai_ac_app = mean(lbsai_ac_app),
            lbsai = sum(lbsai),
            pct_tot = sum(pct_tot)) %>% 
  arrange(-pct_tot) %>% 
  group_by(year, commodity, class) %>% 
  mutate(pct_tot = lbsai/sum(lbsai),
         cum_pct = cumsum(pct_tot)) %>% 
  #--he says 'select products that account for 80% or more of total amount applied'
  #--it seems like he picks the top 4
  slice(1:4) %>% 
  #  select(year, commodity, class, ai2, lbsai_ac_app) %>% 
  # now he picks out the ais from table 2 and puts them in here
  separate(ai2, into = c("ai", "code"), sep = "=") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  left_join(ref_ai_energy %>% 
              select(ai, intern_MJkg)) %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(intern_MJkg),
         btu_ac_app = lbsai_ac_app * energy_btulb) %>% 
  #--now he gets an average weighted by the % it contributes to the total
  group_by(year, commodity, class) %>% 
  summarise(btu_ac_app = weighted.mean(btu_ac_app, pct_tot))

#--the value is off because of rounding, I believe

corn16H %>% 
  bind_rows(corn14H) %>% 
  group_by(commodity, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app))

