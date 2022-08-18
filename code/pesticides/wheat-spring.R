# purpose: create data to feed into Energy Table 3 
# author: Gina Nichols
# created: 8/11/22

# NOTE: using Allison's intern's apporach - could be improved


rm(list = ls())

library(tidyverse)


# startup -----------------------------------------------------------------

source("code/pesticides/functions/StartUp.R")

# specify crop ------------------------------------------------------------

chem %>% 
  pull(crop) %>% 
  unique()

my_comm <- "wheat, spring, (excl durum)"

# herbicides ---------------------------------------------------------

recent_yrs_h <- GetMostRecentTwoYears(f.dat = chem,
                                    f.comm = my_comm,
                                    f.class = "herbicide")

recent_yrs_h

h0 <- 
  chem %>% 
  filter(crop == my_comm,
         class == "herbicide",
         year %in% recent_yrs_h) %>% 
  # lump together certain chemicals, see LumpAis function (to keep consistent across crops)
  LumpAis() %>% 
  arrange(-lbsai) %>% 
  filter(!is.na(lbsai)) %>% 
  group_by(year, crop, class, ai2) %>% 
  #--sum amounts, but averaged the lbs/ac/app
  summarise(lbsai_ac_app = mean(lbsai_ac_app),
            lbsai = sum(lbsai)) %>% 
  group_by(year, crop, class) %>% 
  mutate(pct_tot = lbsai/sum(lbsai)) %>% 
  group_by(year, crop) %>% 
  arrange(year, -pct_tot) %>% 
  group_by(year, crop) %>% 
  mutate(cum_pct = cumsum(pct_tot)) 

ifelse (h0 %>% 
          slice(1:4) %>% 
          pull(cum_pct) %>% 
          max() > 0.80,
        print("4 is good"),
        print("add more"))

h0 %>% 
  filter(cum_pct <0.85) %>% 
  group_by(year) %>% 
  mutate(n = 1:n())

#take top 4 contributors or minimum number to hit >80% cum tot
h1 <- 
  h0%>% 
  group_by(year) %>% 
  slice_min(cum_pct, n = 4)

h1

#--make sure ais are in ref table
h2 <- 
  h1 %>% 
  separate(ai2, into = c("ai", "code"), sep = "=") %>% 
  mutate_if(is.character, str_trim, "both") %>% 
  left_join(ref_ai_energy %>% 
              select(ai, intern_MJkg))


ifelse(sum(is.na(h2 %>%
              pull(intern_MJkg))) >0, 
       print("check"), 
       print("ais ok"))

h3 <- 
  h2 %>% 
  mutate(energy_btulb = ConvMJkgToBTUlb(intern_MJkg),
         btu_ac_app = lbsai_ac_app * energy_btulb) %>% 
  #--now he gets an average weighted by the % it contributes to the total
  group_by(year, crop, class) %>% 
  summarise(btu_ac_app = weighted.mean(btu_ac_app, pct_tot)) %>% 
  group_by(crop, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

h_final <- h3

# fungicides -------------------------------------------------------

recent_yrs_f <- GetMostRecentTwoYears(f.dat = chem,
                                      f.comm = my_comm,
                                      f.class = "fungicide")

recent_yrs_f

#--note we don't have specific values for each ai, so we use the average 
#--(I'm not actually sure how the intern did this. Allison said they 'used expert opinion', which is crap)

f1 <- 
  chem %>% 
  filter(crop == my_comm,
         class == "fungicide",
         year %in% recent_yrs_f) %>% 
  arrange(-lbsai) %>% 
  group_by(year, crop, class) %>% 
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
  group_by(crop, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

f_final <- f3


# insecticides -------------------------------------------------------

recent_yrs_i <- GetMostRecentTwoYears(f.dat = chem,
                                      f.comm = my_comm,
                                      f.class = "insecticide")

recent_yrs_i

#--note we don't have specific values for each ai, so we use the average (?)

i1 <- 
  chem %>% 
  filter(crop == my_comm,
         class == "insecticide",
         year %in% recent_yrs_i) %>% 
  arrange(-lbsai) %>% 
  group_by(year, crop, class) %>% 
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
  group_by(crop, class) %>% 
  summarise(btu_ac_app = mean(btu_ac_app)) 

i_final <- i3

# growth reg -------------------------------------------------------

recent_yrs_g <- GetMostRecentTwoYears(f.dat = chem,
                                      f.comm = my_comm,
                                      f.class = "growth reg")

recent_yrs_g

# #--note we don't have specific values for each ai, so we use the average (?)

# g1 <-
#   chem %>%
#   filter(crop == my_comm,
#          class == "growth reg",
#          year %in% recent_yrs_i) %>%
#   arrange(-lbsai) %>%
#   group_by(year, crop, class) %>%
#   #--sum amounts, but averaged the lbs/ac/app
#   summarise(lbsai_ac_app = mean(lbsai_ac_app),
#             lbsai = sum(lbsai))
# 
# #--make sure ais are in ref table
# g2 <-
#   g1 %>%
#   left_join(ref_class_energy)
# 
# g3 <-
#   g2 %>%
#   mutate(energy_btulb = ConvMJkgToBTUlb(energy_MJkgai),
#          btu_ac_app = lbsai_ac_app * energy_btulb)  %>%
#   group_by(crop, class) %>%
#   summarise(btu_ac_app = mean(btu_ac_app))
# 
# g_final <- g3


#  fumigant -------------------------------------------------------

recent_yrs_fu <- GetMostRecentTwoYears(f.dat = chem,
                                       f.comm = my_comm,
                                       f.class = "fumigant")

recent_yrs_fu

# #--note we don't have specific values for each ai, so we use the average (?)
# 
# fu1 <- 
#   chem %>% 
#   filter(crop == my_comm,
#          class == "fumigant",
#          year %in% recent_yrs_i) %>% 
#   arrange(-lbsai) %>% 
#   group_by(year, crop, class) %>% 
#   #--sum amounts, but averaged the lbs/ac/app
#   summarise(lbsai_ac_app = mean(lbsai_ac_app),
#             lbsai = sum(lbsai)) 
# 
# #--make sure ais are in ref table
# fu2 <- 
#   fu1 %>% 
#   left_join(ref_class_energy)
# 
# fu3 <- 
#   fu2 %>% 
#   mutate(energy_btulb = ConvMJkgToBTUlb(energy_MJkgai),
#          btu_ac_app = lbsai_ac_app * energy_btulb)  %>% 
#   group_by(crop, class) %>% 
#   summarise(btu_ac_app = mean(btu_ac_app)) 
# 
# fu_final <- fu3


# final -------------------------------------------------------------------

hif_years <- 
  c(glue_collapse(recent_yrs_h, sep = " "),
    glue_collapse(recent_yrs_i, sep = " "),
    glue_collapse(recent_yrs_f, sep = " "))


res <-
  h_final %>% 
  bind_rows(i_final) %>% 
  bind_rows(f_final) %>% 
  mutate(years_of_data = as.character(hif_years))

res %>% 
  write_csv("data_tidy/pest_energy_wheat-spring.csv")
