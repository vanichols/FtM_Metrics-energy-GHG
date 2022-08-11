#--start up code

library(tidyverse)
library(glue)

# get necessary NASS data -------------------------------------------------

load("data_clean-nass/clean_nass_data.rda")

rm(corn_pct, nass_land_use_final, nass_num_passes_final)

#--load helper functions
source("code/pesticides/functions/CleanRawChemData.R")
source("code/pesticides/functions/GetMostRecentTwoYears.R")
source("code/pesticides/functions/Conversions.R")
source("code/pesticides/functions/LumpAis.R")


# reference table that needs updated --------------------------------------

ref_ai_energy <- 
  read_csv("data_references/audsely2009-table2-ais.csv", skip = 5)

ref_class_energy <- 
  read_csv("data_references/audsley2009-table2-class.csv", skip = 5)


# chem data ---------------------------------------------------------------

chem <- CleanRawChemData(f.dapps = nass_apps_final,
                         f.dchems = nass_chemicals_final)


