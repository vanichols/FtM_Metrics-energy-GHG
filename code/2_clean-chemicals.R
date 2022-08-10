# purpose Clean chemical data, taken from NIR cleaning porcess, needs to be adjusted for metrics purposes
# author: Eric Coronel, Gina
# date created: 3/25/2021
# modified: 8/10/22 - making into R script instead of Rmarkdown

# R Packages And Other Global Options

library(tidyverse)
library(readxl)
library(usdarnass)

load("data_raw/nass_data.rda")


# clean chemicals ---------------------------------------------------------

# what I see I need to do
# Separate chemical data into fertilizer and chemicals
# Fertilizer processing is the same, but with the removal of sulfur data
# Separate into data < 1994 and data >= 1994
# Separate chemical data into fungicide + herbicide + insecticide and OTHER
# For data < 1994
# Add ref table to partition the chemical, others appropriately, will need new columns
# Sum up data together by chemical category - there will be no Other category, we only keep the categories for which we have multiplication factors
# For data >= 1994
# We add ref table data to again be able to distribute CHEMICAL, OTHER data into appropriate categories
# Then we add these new totals into the respective total category (herbicide, fumigant, etc). There will be no Other category but we'll have new ones (fumigants, growth regulators)

# Import updated ref table
ref_table_pest <- read_csv("data_references/pesticide_codes.csv")

#### Chemicals < 1994 ####
# Before 1994 there were different 'classes'
# Separate chemical data into fungicide + herbicide + insecticide and OTHER
# For data < 1994
# Add ref table to partition the chemical, others appropriately, will need new columns
# Sum up data together by chemical category - there will be no Other category, 
# we only keep the categories for which we have multiplication factors

#--note not every chemical is in our reference list, but if it isn't on the list, it already has a class so it doesn't matter
#--here we are replacing NASS 'OTHER" class with our own class from the reference list



chemicals_before_1994 <- 
  nass_chemicals %>% 
  filter(Domain != "FERTILIZER") %>% 
  filter(Year < 1994) %>% 
  # Separate to get chemical code to do a left join
  mutate(`Domain Category` = str_remove_all(`Domain Category`, "\\(|\\)")) %>% 
  #--get the code isolated so we can join it
  separate(`Domain Category`, into = c("ai", "code"), sep = " = ") %>% 
  mutate(code = as.numeric(code)) %>% 
  left_join(ref_table_pest, by = "code") %>% 
  mutate(type_edited = paste("CHEMICAL,", toupper(type))) %>% 
  mutate(Domain = ifelse(test = Domain == "CHEMICAL, OTHER",
                         yes = type_edited,
                         no = Domain)) %>% 
  group_by(Year, Commodity, Domain) %>% 
  summarize(Value = sum(Value)) # This is ready

#### post 1994 ####

# For data >= 1994
# We add ref table data to again be able to distribute CHEMICAL, OTHER data into appropriate categories
# Then we add these new totals into the respective total category (herbicide, fumigant, etc). There will be no Other category but we'll have new ones (fumigants, growth regulators)
# Chemicals >= 1994

# Excludes CHEMICAL, OTHER
chemicals_after_1994_totals_only <- 
  nass_chemicals %>% 
  filter(Domain != "FERTILIZER") %>% 
  filter(Year >= 1994) %>% 
  filter(str_detect(`Domain Category`, "TOTAL")) %>% 
  filter(Domain != "CHEMICAL, OTHER")


#--assign a class to things that are classified as 'other' in NASS
#--we only keep the top four classes by pounds
chemicals_after_1994_others_all_classes <- 
  nass_chemicals %>% 
  filter(Domain != "FERTILIZER") %>% 
  filter(Year >= 1994) %>% 
  filter(Domain == "CHEMICAL, OTHER") %>% 
  filter(`Domain Category` != "CHEMICAL, OTHER: (TOTAL)") %>% 
  filter(!is.na(Value)) %>% 
  mutate(`Domain Category` = str_remove_all(`Domain Category`, "\\(|\\)")) %>% 
  separate(`Domain Category`, into = c("ai", "code"), sep = " = ") %>% 
  mutate(code = as.numeric(code)) %>% 
  left_join(ref_table_pest, by = "code") %>% 
  ungroup() %>% 
  group_by(Year, Commodity, type) %>% 
  summarize(Value = sum(Value))

#--six classes at this point
chemicals_after_1994_others_all_classes %>% 
  pull(type) %>% 
  unique()

chemicals_after_1994_others_all_classes %>% 
  group_by(type) %>% 
  summarise(Value = sum(Value)) %>% 
  arrange(-Value)

#--we keep the top four classes (not sure why)
chemicals_after_1994_others_only <-
  chemicals_after_1994_others_all_classes %>% 
  filter(type %in% c("Fumigant", "Fungicide", "Herbicide", "Growth Reg")) 

#### manual replacements ####
# potatoes 1996 gets 43869000 lbs of fumigants
# peanuts 2013 gets 1/3 of Fumigant, Fungicide, Growth Reg for 1303000 lbs
# wheat 2015 gets 29000 lbs of 2015 divided as Fungicide 0.4, Growth Reg 0.2, Herbicide 0.4
# for source data check "sleuthing_chemicals_others.R" in primitives folder
# It's to make up for unaccounted chemical others
additional_chem_other_data <- 
  data.frame(
  Year = c(1996, 2013, 2013, 2013, 2015, 2015, 2015),
  Commodity = c("POTATOES", "PEANUTS", "PEANUTS", "PEANUTS", "WHEAT", "WHEAT", "WHEAT"),
  type = c("Fumigant", "Fumigant", "Fungicide", "Growth Reg", "Fungicide", "Growth Reg", "Herbicide"),
  Value = c(43869000, (0.33 * 1303000), (0.33 * 1303000), (0.33 * 1303000), 
            (0.4 * 29000), (0.2 * 29000), (0.4 * 29000))
)

chemicals_after_1994_others_only_with_addition <- 
  chemicals_after_1994_others_only %>% 
  bind_rows(additional_chem_other_data) %>% 
  arrange(Year, Commodity) %>% 
  mutate(Domain = paste("CHEMICAL,", toupper(type))) %>% 
  select(Year, Commodity, Domain, Value)

# I now need to add chemicals_after_1994_others_only_with_addition to the >1994 dataset 
# Also add the data before 1994 and the fertilizer data
# and do one more sum by category, plus some cosmetics probably
chem_processing <- 
  chemicals_after_1994_totals_only %>% 
  select(-c(`Data Item`, `Domain Category`)) %>% 
  bind_rows(chemicals_after_1994_others_only_with_addition) %>% 
  bind_rows(chemicals_before_1994) %>%
  ungroup() %>% 
  group_by(Year, Commodity, Domain) %>% 
  summarize(Value = sum(Value)) %>%
  arrange(Year, Commodity)

# select variables
nass_chemicals_2 <- 
  chem_processing %>% 
  rename(crop = Commodity) %>% 
  rename_all(tolower) %>% 
  #separate(`data item`, c("crop", "item_orig"), sep = " - ") %>% 
  #filter(item_orig == "APPLICATIONS, MEASURED IN LB") %>% 
  mutate(#value = as.numeric(str_remove_all(value, ",")),
    item = case_when(domain == "CHEMICAL, FUNGICIDE" ~ "chemical_fungicide",
                     domain == "CHEMICAL, HERBICIDE" ~ "chemical_herbicide",
                     domain == "CHEMICAL, INSECTICIDE" ~ "chemical_insecticide",
                     domain == "CHEMICAL, FUMIGANT" ~ "chemical_fumigant",
                     domain == "CHEMICAL, GROWTH REG" ~ "chemical_growth_reg",
                     domain == "FERTILIZER: (NITROGEN)" ~ "fertilizer_nitrogen",
                     domain == "FERTILIZER: (PHOSPHATE)" ~ "fertilizer_phosphate",
                     domain == "FERTILIZER: (POTASH)" ~ "fertilizer_potash"
    )) %>% 
  #filter(!is.na(item)) %>% 
  mutate(crop = str_to_sentence(crop) %>% str_replace("_", ", "))

# reshape & sum by major crop
nass_chemicals_3 <- 
  nass_chemicals_2 %>% 
  select(year, crop, item, value) %>% 
  pivot_wider(names_from = item,
              values_from = value) %>% 
  mutate(crop = case_when(#crop == "Potatoes, fall" ~ "Potatoes",
    #crop == "Wheat, spring, (excl durum)" ~ "Wheat",
    #crop == "Wheat, spring, durum" ~ "Wheat",
    #crop == "Wheat, winter" ~ "Wheat",
    #crop == "Wheat, post harvest" ~ "Wheat", # only shows up in 2009 - delete?
    crop == "Sugarbeets" ~ "Sugar beets",
    TRUE ~ crop)) #%>% 
#group_by(year, crop) %>% 
#summarise_all(sum, na.rm = TRUE)

# assume same chemical use rate for corn grain and corn silage
corn_grain <- 
  nass_chemicals_3 %>% 
  filter(crop == "Corn") %>% 
  mutate(crop = "Corn, grain")

corn_silage <- 
  nass_chemicals_3 %>% 
  filter(crop == "Corn") %>% 
  mutate(crop = "Corn, silage")

corn <- 
  bind_rows(corn_grain, corn_silage) %>% 
  ungroup() %>% 
  mutate(year = as.numeric(year)) %>% 
  left_join(corn_pct, by = c("year", "crop")) %>%  #---need this from land-cleaning
  mutate_at(vars(-year, -crop), list(~ . * pct_corn_acres)) %>% 
  select(-pct_corn_acres)

nass_chemicals_4 <- 
  nass_chemicals_3 %>% 
  ungroup() %>% 
  filter(crop != "Corn") %>% 
  mutate(year = as.numeric(year)) %>% 
  bind_rows(corn)

nass_chemicals_final <- 
  nass_chemicals_4


nass_chemicals_final %>% 
  write_csv("data_tidy/nass_chemicals_final.csv")
