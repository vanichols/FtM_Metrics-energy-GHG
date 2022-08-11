# purpose: create data to feed into Energy Table 3 
# author: Gina Nichols
# created: 8/11/22

rm(list = ls())

library(tidyverse)
library(fs)
library(ddpcr)


# run code to make sure it's the most recent years of data ----------------

# quiet(source("code/pesticides/barley.R"))
# quiet(source("code/pesticides/corn.R"))
# quiet(source("code/pesticides/cotton-upland.R"))
# quiet(source("code/pesticides/cotton.R"))
# quiet(source("code/pesticides/peanuts.R"))
# quiet(source("code/pesticides/potatoes.R"))
# quiet(source("code/pesticides/rice.R"))
# quiet(source("code/pesticides/sorghum.R"))
# quiet(source("code/pesticides/soybeans.R"))
# quiet(source("code/pesticides/sugarbeets.R"))
# quiet(source("code/pesticides/wheat-durum.R"))
# quiet(source("code/pesticides/wheat-spring.R"))
# quiet(source("code/pesticides/wheat-winter.R"))
# 


# read in pesticide energy files ------------------------------------------

#--fucking sugar beets, read them in separately
pest_energy_files <- dir_ls("data_tidy/")[-10]

d1 <- 
  pest_energy_files %>% 
  map_dfr(read_csv)

pest_energy_files2 <- dir_ls("data_tidy/")[10]

d2 <- 
  pest_energy_files2 %>% 
  map_dfr(read_csv) %>% 
  mutate(years_of_data = as.character(years_of_data))


d <- 
  d1 %>% 
  bind_rows(d2)


# wrangle -----------------------------------------------------------------

d %>% 
  ggplot(aes(class, btu_ac_app)) + 
  geom_jitter(aes(color = crop), size = 3, width = 0.1)



# holes -------------------------------------------------------------------

# we include seed treatments, growth regulators, fumigants, and harvest aids
# I have no idea how we got those numbers...another week down the drain probably
