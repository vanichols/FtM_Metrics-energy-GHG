# purpose: Use NASS chemical data
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



# load in nass data -------------------------------------------------------

nass_chemicals <- read_csv("CHEM_nass-chemicals.csv")

corn_pct <- read_csv("CHEM_landuse-corn-types-pct.csv")

nass_land_use <- read_csv("CHEM_nass_land_use_final.csv")


# clean nass chemical data ---------------------------------------------------------


# NASS chemicals
# Remove fertilizer from lists
# Separate into data < 1994 and data >= 1994


# Import updated ref table
ref_table_pest <- read_csv("data_references/pesticide_codes.csv")


#### before 1994 ####
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

# manual replacements
# potatoes 1996 gets 43869000 lbs of fumigants
# peanuts 2013 gets 1/3 of Fumigant, Fungicide, Growth Reg for 1303000 lbs
# wheat 2015 gets 29000 lbs of 2015 divided as Fungicide 0.4, Growth Reg 0.2, Herbicide 0.4
# for source data check "sleuthing_chemicals_others.R" in primitives folder
# It's to make up for unaccounted chemical others
additional_chem_other_data <- data.frame(
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
# Also add the data before 1994 and the fertilizer data (GN - eliminated this for the chemical use stand-alone code)
# and do one more sum by category, plus some cosmetics probably
chem_processing <- 
  chemicals_after_1994_totals_only %>% 
  select(-c(`Data Item`, `Domain Category`)) %>% 
  bind_rows(chemicals_after_1994_others_only_with_addition) %>% 
  bind_rows(chemicals_before_1994) %>%
  #bind_rows(fertilizers_all_years) %>% 
  ungroup() %>% 
  group_by(Year, Commodity, Domain) %>% 
  summarize(Value = sum(Value)) %>%
  arrange(Year, Commodity)

#### formatting ####

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
                     #domain == "FERTILIZER: (NITROGEN)" ~ "fertilizer_nitrogen",
                     #domain == "FERTILIZER: (PHOSPHATE)" ~ "fertilizer_phosphate",
                     #domain == "FERTILIZER: (POTASH)" ~ "fertilizer_potash"
    )) %>% 
  #filter(!is.na(item)) %>% 
  mutate(crop = str_to_sentence(crop) %>% str_replace("_", ", "))

# reshape & sum by major crop
nass_chemicals_3 <- 
  nass_chemicals_2 %>% 
  select(year, crop, item, value) %>% 
  spread(key = "item", value = "value") %>% 
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
  left_join(corn_pct, by = c("year", "crop")) %>% 
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


# calculate embedded energy by class/crop --------------------------------------------------------


#### calculate conversions ####

# Below are direct conversions from Audsley 2009 Table 1
#--given in MJ/kg

# MJ/kg ai are converted to BTU/lb ai by multiplying by (947.8/2.20462)
# 1 MJ to BTU	947.8
# 1 kg to lb	2.20462
MJkg_to_BTUlb <- 947.8/2.20462

c_fungicide_btu_lb <- 423 * MJkg_to_BTUlb
c_herbicide_btu_lb <- 386 * MJkg_to_BTUlb
c_insecticide_btu_lb <- 274 * MJkg_to_BTUlb 
c_growth_reg_btu_lb <- 276 * MJkg_to_BTUlb
c_fumigant_btu_lb <- 386 * MJkg_to_BTUlb # assumed to be same as herbicide



# lb CO2e/lb ai are obtained as Audsley 2009 suggested
# A factor of 0.069 kg CO2 equivalent per MJ pesticide energy can be used to convert these to the Global Warming Potential (100 years). Since the units are kg CO2e/kg ai, it's the same as lb CO2e/lb ai
co2e_conv <- 0.069

c_fungicide_co2e_lb <- 423 * co2e_conv
c_herbicide_co2e_lb <- 386 * co2e_conv
c_insecticide_co2e_lb <- 274 * co2e_conv
c_growth_reg_co2e_lb <- 276 * co2e_conv
c_fumigant_co2e_lb <- 386 * co2e_conv # same as herbicide


#--this is new, GN
chem_energy <- 
  tibble(
  class = c("fungicide", "herbicide", "insecticide", "growth_reg", "fumigant"),
  energy_BTUlb = c(c_fungicide_btu_lb, c_herbicide_btu_lb, c_insecticide_btu_lb, c_growth_reg_btu_lb, c_fumigant_btu_lb),
  ghg_co2elb = c(c_fungicide_co2e_lb, c_herbicide_co2e_lb, c_insecticide_co2e_lb, c_growth_reg_co2e_lb, c_fumigant_co2e_lb)
)

#### use most recent two years of data available for each crop/class ####

#--two most recent years?
recent_2year_chemical <- 
  nass_chemicals_final %>% 
  pivot_longer(3:ncol(.)) %>% 
  mutate(name = str_remove(name, "chemical_")) %>% 
  filter(!is.na(value)) %>% 
  arrange(-year) %>% 
  group_by(crop, name) %>% 
  slice_max(order_by = year, n = 2) %>% 
  select(-value)
  

recent_2year_chemical_table <- 
  recent_2year_chemical %>% 
  group_by(crop, name) %>% 
  filter(year == max(year)) %>% 
  rename("max_year" = year) %>% 
  left_join(
    recent_2year_chemical %>% 
      group_by(crop, name) %>% 
      filter(year == min(year)) %>% 
      rename("min_year" = year) 
    ) %>% 
  mutate(
    years = paste(min_year, max_year, sep = ", ")) %>% 
  select(-max_year, -min_year) %>% 
  pivot_wider(names_from = name,
              values_from = years)

recent_2year_chemical_table %>% 
  write_csv("CHEM_most-recent-2years-of-data.csv")

#--keep the two most recent years of data and average them
chemical_recent2years <- 
  nass_chemicals_final %>% 
  pivot_longer(3:ncol(.)) %>% 
  mutate(name = str_remove(name, "chemical_")) %>% 
  filter(!is.na(value)) %>% 
  arrange(-year) %>% 
  group_by(crop, name) %>% 
  slice_max(order_by = year, n = 2) %>% 
  left_join(nass_land_use) %>% 
  group_by(crop, name) %>% 
  summarise(pounds_ai = mean(value),
            acres_planted = mean(acres_planted),
            acres_harvested = mean(acres_harvested))
  
#### combine ####

chemical_recent2years %>% 
  rename("class" = name) %>% 
  mutate(
    pounds_acharv = pounds_ai/acres_harvested,
    pounds_acpl = pounds_ai/acres_planted
    ) %>% 
  left_join(chem_energy) %>% 
  mutate(
    energy_BTUacharv = pounds_acharv * energy_BTUlb,
    energy_BTUacpl = pounds_acpl * energy_BTUlb
  ) %>% 
  select(crop, class, energy_BTUacpl) %>% 
  pivot_wider(names_from = class, 
              values_from = energy_BTUacpl)




# Very important step, all NAs converted to 0 to be able to complete row sums
# Let's keep an eye open for unintended consequences
input[is.na(input)] <- 0

input_2 <- input %>% 
  # Creating water applied in acre inches instead of acre feet
  mutate(water_applied_acre_inches = water_applied * 12) %>%
  # Creating delta for irrigated - non-irrigated yield
  mutate(yield_irrigation_delta = yield_irrigated - yield_non_irrigated,
         production_irrigation_delta = acres_irrigated * yield_irrigation_delta) %>% 
  # Merging with multipliers and other important factors
  left_join(factors, by = "crop")

# Calculating the rest of the metrics
input_3 <- input_2 %>% 

    #### chemicals ####
    
    # unit: total BTUs, massive number, we'll divide by a million or billion at the very end
    herbicide_total_BTUs = chemical_herbicide * c_herbicide_btu_lb,
    insecticide_total_BTUs = chemical_insecticide * c_insecticide_btu_lb,
    fungicide_total_BTUs = chemical_fungicide * c_fungicide_btu_lb,
    growth_reg_total_BTUs = chemical_growth_reg * c_growth_reg_btu_lb,
    fumigant_total_BTUs = chemical_fumigant * c_fumigant_btu_lb,
    
    chemicals_total_BTUs = (herbicide_total_BTUs + insecticide_total_BTUs + fungicide_total_BTUs +
                              growth_reg_total_BTUs + fumigant_total_BTUs),
    
    chemicals_BTUs_per_acre = chemicals_total_BTUs / acres_planted,
    
    chemicals_total_BTUs_global_adjustment = chemicals_total_BTUs * global_energy_efficiency_multiplier,
    
    chemicals_BTUs_per_acre_global_adjustment = chemicals_total_BTUs_global_adjustment / acres_planted,
    
    # unit: total CO2e, massive number, we'll divide by a million or billion at the very end
    herbicide_total_CO2e = chemical_herbicide * c_herbicide_co2e_lb,
    insecticide_total_CO2e = chemical_insecticide * c_insecticide_co2e_lb,
    fungicide_total_CO2e = chemical_fungicide * c_fungicide_co2e_lb,
    growth_reg_total_CO2e = chemical_growth_reg * c_growth_reg_co2e_lb,
    fumigant_total_CO2e = chemical_fumigant * c_fumigant_co2e_lb,
    
    chemicals_total_CO2e = (herbicide_total_CO2e + insecticide_total_CO2e + fungicide_total_CO2e +
                              growth_reg_total_CO2e + fumigant_total_CO2e),
    
    chemicals_CO2e_per_acre = chemicals_total_CO2e / acres_planted,
    
    chemicals_total_CO2e_global_adjustment = chemicals_total_CO2e * CO2_intensity_multiplier,
    
    chemicals_CO2e_per_acre_global_adjustment = chemicals_total_CO2e_global_adjustment / acres_planted,
    
    #### fertilizers ####
    
    # unit: total BTUs, massive number, we'll divide by a million or billion at the very end
    nitrogen_total_BTUs = fertilizer_nitrogen * nitrogen_btu_lb,
    phosphorus_total_BTUs = fertilizer_phosphate * phosphorus_btu_lb,
    potassium_total_BTUs = fertilizer_potash * potassium_btu_lb,
    
    fertilizer_total_BTUs = (nitrogen_total_BTUs + phosphorus_total_BTUs + potassium_total_BTUs),
    
    fertilizer_BTUs_per_acre = fertilizer_total_BTUs / acres_planted,
    
    fertilizer_total_BTUs_adjusted = ((nitrogen_total_BTUs * IFA_efficiency_multiplier) + 
                                        (phosphorus_total_BTUs * global_energy_efficiency_multiplier) +
                                        (potassium_total_BTUs * global_energy_efficiency_multiplier)),
    
    fertilizer_BTUs_per_acre_adjusted = fertilizer_total_BTUs_adjusted / acres_planted,
    
    # unit: total CO2e, massive number, we'll divide by a million or billion at the very end
    nitrogen_total_CO2e = fertilizer_nitrogen * nitrogen_co2_lb,
    phosphorus_total_CO2e = fertilizer_phosphate * phosphorus_co2_lb,
    potassium_total_CO2e = fertilizer_potash * potassium_co2_lb,
    
    fertilizer_total_CO2e = (nitrogen_total_CO2e + phosphorus_total_CO2e + potassium_total_CO2e),
    
    fertilizer_CO2e_per_acre = fertilizer_total_CO2e / acres_planted,
    
    fertilizer_total_CO2e_adjusted = ((nitrogen_total_CO2e * IFA_efficiency_multiplier) + 
                                        (phosphorus_total_CO2e * CO2_intensity_multiplier) + 
                                        (potassium_total_CO2e * CO2_intensity_multiplier)),
    
    fertilizer_CO2e_per_acre_adjusted = fertilizer_total_CO2e_adjusted / acres_planted,
    
    #### seed ####
    # unit: million BTUs
    #energy_use_seed = (energy_use_machinery + energy_use_irrigation + energy_use_drying + energy_use_chemicals +
    #                     energy_use_fertilizers) * c_seed_input_intensity_factor * c_seed_yield_factor,
    # I updated with current metrics methods
    
    seed_BTUs_per_acre = (seeding_rate_lbs_per_acre / c_wt_lbs) * c_seed_btu_yield_unit,
    
    seed_total_BTUs = seed_BTUs_per_acre * acres_planted,
    
    seed_CO2e_per_acre = (seeding_rate_lbs_per_acre / c_wt_lbs) * c_seed_CO2e_yield_unit,
    
    seed_total_CO2e = seed_CO2e_per_acre * acres_planted,
    
    #### transportation ####
    # updated with current metrics methodology 04/16/2021
    # assumes diesel for fuel, mileage and truck capacity listed in factors data
    # We are assuming a round-trip with impact of 1.8 instead of 2, to account for fuel efficiency when truck runs #empty in the way back
    truck_miles_per_gallon = 6.5,
    truck_round_trip_coeff = 1.8, # 1 trip + 0.8 trip since the truck in return trip comes back empty
    
    transportation_total_BTUs = (production / c_truck_capacity_yield_output) * ((c_transportation_distance/truck_miles_per_gallon)*gal_diesel_to_BTU) * truck_round_trip_coeff,
    
    transportation_BTUs_per_acre = transportation_total_BTUs / acres_planted,
    
    transportation_total_CO2e = transportation_total_BTUs / gal_diesel_to_BTU * gal_diesel_to_CO2,
    
    transportation_CO2e_per_acre = transportation_total_CO2e / acres_planted,
    
    #### nitrous oxide ####
    # n2o_emissions_factor is the N2O emissions per lb of nitrogen applied, updated from last report, when it was 0.014
    # 44/28 = the molecular weight ratio of N2O/N2O-N
    # Global Warming Potential of N2O = 298, https://www.epa.gov/ghgemissions/overview-greenhouse-gases#N2O-references
    n2o_CO2e_per_acre_primitive = (((fertilizer_nitrogen/acres_planted) + manure_N_lbs_per_acre) * n2o_emissions_factor * (44/28) * 298),
    
    n2o_total_CO2e_primitive = n2o_CO2e_per_acre_primitive * acres_planted, 
    
    # updated equations for N2O
    # Equation 11.1 Direct N2O emissions from managed soils (Tier 1)
    n2o_eq_11_1_per_acre = ifelse(crop == "Rice",
                                  #(((fertilizer_nitrogen/acres_planted) + manure_N_lbs_per_acre) * 0.010) + 
                                  (((fertilizer_nitrogen/acres_planted) + manure_N_lbs_per_acre) * 0.004),
                                  ((fertilizer_nitrogen/acres_planted) + manure_N_lbs_per_acre) * 0.010),
    
    # Equation 11.9 N2O from atmospheric deposition of N volatilized from managed soils (Tier 1) 11.22, IPCC 2019
    n2o_eq_11_9_per_acre = (((fertilizer_nitrogen/acres_planted) * 0.11) + (manure_N_lbs_per_acre * 0.21)) * 0.010,
    
    # Equation 11.10 N2O from N leaching/runoff from managed soils in regions where leaching/runoff occurs (Tier 1)
    n2o_eq_11_10_per_acre = (((fertilizer_nitrogen/acres_planted) + manure_N_lbs_per_acre) * 0.24) * 0.011,
    
    # Aggregating all three equations above
    n2o_CO2e_per_acre = (n2o_eq_11_1_per_acre + n2o_eq_11_9_per_acre + n2o_eq_11_10_per_acre) * (44/28) * 298,
    
    n2o_total_CO2e = n2o_CO2e_per_acre * acres_planted, 
    
    #### residue burning ####
    ## when divided per acre the number gets highly diluted
    # This indicator was also updated to match current metrics methodology
    
    residue_burning_total_CO2e = acres_planted * (percent_area_burned/100) * yield_planted * c_residue_burning_co2e_lb,
    
    residue_burning_CO2e_per_acre = residue_burning_total_CO2e / acres_planted,
    
    #### Methane emissions for Rice ####
    rice_methane_total_CO2e = ifelse(crop == "Rice", 
                                     methane_CO2e_acre * acres_harvested,
                                     no = 0),
    
    rice_methane_CO2e_per_acre = rice_methane_total_CO2e / acres_planted,
    
    ##### Residue removal credit for Wheat and Barley ####
    # remember to divide acreage share by 100
    # https://ocj.com/2011/07/determining-the-actual-nutrient-value-of-wheat-straw/
    # Facts and assumptions about this residue removal credit
    # Bushel of wheat 60 lbs, bushel of barley 48 lbs
    # Percent N of straw: 0.005 (0.5 percent)
    # Grain to straw production ratio: 1 (very conservative value)
    # Straw production from a bushel of wheat/barley: 60/48 lbs, same as weight of grain (water not included here)
    # N content from pound of straw/wheat = 0.3 lbs N (60 lbs * 0.005)
    # N content from pound of straw/barley = 0.24 lb N (48 lbs * 0.005)
    # Both factors above get divided by 2 (assumption of 50% residue removal)
    # n2o_emissions_factor is the IPCC N loss factor, just 1% basically
    # 298 is the warming potential of N2O, https://www.epa.gov/ghgemissions/overview-greenhouse-gases#N2O-references
    # 44/28 is the Ratio of N2O to N2O-N
    # The number is made negative to subtract from the final value
    residue_removal_credit_total_CO2e = case_when(
      crop == "Wheat" ~ production * (residue_removal_percent_acreage/100) * (0.3/2) * n2o_emissions_factor *
        298 * (44/28) * -1,
      crop == "Barley" ~ production * (residue_removal_percent_acreage/100) * (0.24/2) * n2o_emissions_factor *
        298 * (44/28) * -1,
      TRUE ~ 0
    ),
    
    # To check impact per acre
    residue_removal_credit_CO2e_per_acre = residue_removal_credit_total_CO2e / acres_planted,
    
    #### Application energy ####
    application_total_BTUs = total_application_passes * application_pass_btu * acres_planted,
    
    application_BTUs_per_acre = application_total_BTUs / acres_planted, 
    
    # Application emissions
    application_total_CO2e = (application_total_BTUs / gal_diesel_to_BTU) * gal_diesel_to_CO2,
    
    application_CO2e_per_acre = application_total_CO2e / acres_planted,
    
    #### Total BTUs ####
    total_BTUs_unadjusted = management_total_BTUs + irrigation_total_BTUs + drying_total_BTUs + chemicals_total_BTUs +
      fertilizer_total_BTUs + seed_total_BTUs + transportation_total_BTUs + application_total_BTUs,
    # Calculating BTUs per acreage planted
    #BTUs_per_acre_unadjusted = total_BTUs_unadjusted / acres_planted, # this causes a discrepancy due to irrigation
    BTUs_per_acre_unadjusted = application_BTUs_per_acre + chemicals_BTUs_per_acre + drying_BTUs_per_acre +
      fertilizer_BTUs_per_acre + irrigation_BTUs_per_acre + management_BTUs_per_acre + seed_BTUs_per_acre +
      transportation_BTUs_per_acre, 
    # Calculating BTUs per yield unit
    BTUs_per_yield_unit_unadjusted = total_BTUs_unadjusted / production,
    
    # Total adjusted
    total_BTUs_adjusted = management_total_BTUs + irrigation_total_BTUs + drying_total_BTUs +
      chemicals_total_BTUs_global_adjustment + fertilizer_total_BTUs_adjusted + seed_total_BTUs +
      transportation_total_BTUs + application_total_BTUs,
    # Per acre adjusted
    #BTUs_per_acre_adjusted = total_BTUs_adjusted / acres_planted, # this causes a discrepancy due to irrigation
    BTUs_per_acre_adjusted = application_BTUs_per_acre + chemicals_BTUs_per_acre_global_adjustment +
      drying_BTUs_per_acre + fertilizer_BTUs_per_acre_adjusted + irrigation_BTUs_per_acre + management_BTUs_per_acre +
      seed_BTUs_per_acre + transportation_BTUs_per_acre,
    
    # Per yield unit adjusted
    BTUs_per_yield_unit_adjusted = total_BTUs_adjusted / production,
    
    #### Total CO2e ####
    # Notice that we use irrigation_total_CO2e_grid_adjusted, drying_total_CO2e_eGRID_adjusted, chemicals_total_co2e_grid_adjusted, and fertilizer_total_co2e_grid_adjusted to account for eGRID efficiency over time
    total_CO2e_adjusted = management_total_CO2e + irrigation_total_CO2e_grid_adjusted +
      drying_total_CO2e_eGRID_adjusted + chemicals_total_CO2e_global_adjustment + fertilizer_total_CO2e_adjusted +
      n2o_total_CO2e + seed_total_CO2e + transportation_total_CO2e + residue_burning_total_CO2e +
      rice_methane_total_CO2e + residue_removal_credit_total_CO2e + application_total_CO2e,
    # Calculating CO2e per acre
    #CO2e_per_acre_adjusted = total_CO2e_adjusted / acres_planted,# this causes a discrepancy due to irrigation
    CO2e_per_acre_adjusted = application_CO2e_per_acre + chemicals_CO2e_per_acre_global_adjustment +
      drying_CO2e_per_acre_eGRID_adjusted + fertilizer_CO2e_per_acre_adjusted + irrigation_CO2e_grid_adjusted_per_acre
    + management_CO2e_per_acre + n2o_CO2e_per_acre + residue_burning_CO2e_per_acre +
      residue_removal_credit_CO2e_per_acre + rice_methane_CO2e_per_acre + seed_CO2e_per_acre +
      transportation_CO2e_per_acre,
    # Calculating CO2e per yield unit
    CO2e_per_yield_unit_adjusted = total_CO2e_adjusted / production,
    
    # Unadjusted below for comparison
    total_CO2e_unadjusted = management_total_CO2e + irrigation_total_CO2e + drying_total_CO2e +
      chemicals_total_CO2e + fertilizer_total_CO2e + n2o_total_CO2e + seed_total_CO2e + transportation_total_CO2e + residue_burning_total_CO2e + rice_methane_total_CO2e + residue_removal_credit_total_CO2e + application_total_CO2e,
    # Calculating CO2e per acre
    #CO2e_per_acre_unadjusted = total_CO2e_unadjusted / acres_planted,# this causes a discrepancy due to irrigation
    CO2e_per_acre_unadjusted = application_CO2e_per_acre + chemicals_CO2e_per_acre + drying_CO2e_per_acre +
      fertilizer_CO2e_per_acre + irrigation_CO2e_per_acre + management_CO2e_per_acre + n2o_CO2e_per_acre +
      residue_burning_CO2e_per_acre + residue_removal_credit_CO2e_per_acre + rice_methane_CO2e_per_acre +
      seed_CO2e_per_acre + transportation_CO2e_per_acre,
    # Calculating CO2e per yield unit
    CO2e_per_yield_unit_unadjusted = total_CO2e_unadjusted / production
  )

# Importing ref table for the many components and units
indicators_components <- read_excel("data_references/indicators and components.xlsx")

# Pulling vector of totals
indicators_components_vector <- indicators_components %>% 
  filter(indicator_type %in% c("Energy", "Emissions")) %>% 
  pull(totals)

# Calculating per-yield-unit variables
input_4 <- input_3 %>% 
  mutate_at(indicators_components_vector,
            .funs = list(PYU_per_yield_unit = ~ ./production))

# Renaming
names(input_4) <- str_replace_all(string = names(input_4), pattern = "total_BTUs_PYU", replacement = "BTUs")
names(input_4) <- str_replace_all(string = names(input_4), pattern = "total_CO2e_PYU", replacement = "CO2e")
#View(names(input_4))

# More renaming
input_4 <- input_4 %>% 
  rename(irrigation_CO2e_grid_adjusted_per_yield_unit = irrigation_total_CO2e_grid_adjusted_PYU_per_yield_unit,
         chemicals_CO2e_global_adjustment_per_yield_unit = chemicals_total_CO2e_global_adjustment_PYU_per_yield_unit,
         fertilizer_CO2e_adjusted_per_yield_unit = fertilizer_total_CO2e_adjusted_PYU_per_yield_unit,
         chemicals_BTUs_global_adjustment_per_yield_unit  = chemicals_total_BTUs_global_adjustment_PYU_per_yield_unit,
         fertilizer_BTUs_adjusted_per_yield_unit = fertilizer_total_BTUs_adjusted_PYU_per_yield_unit,
         drying_CO2e_eGRID_adjusted_per_yield_unit = drying_total_CO2e_eGRID_adjusted_PYU_per_yield_unit
  )

# Checking names
#View(names(input_4))

# Now run cotton 83 pct allocation.R and create cotton_input_4
# Final data frame
NIR_calculated <- input_4 %>% 
  bind_rows(cotton_input_4)

# Exporting final data frame
save(NIR_calculated, file = "r_data/outputs_04_egc.rda")
```

# 05 Dataframes For Shiny App

```{r shiny}

# Load indicators
load("r_data/outputs_04_egc.rda")

# Importing ref table for the many components and units
indicators_components <- read_excel("data_references/indicators and components.xlsx")

# Layout for shiny
# The major indicators
# then partitioned by crops and categories, values and proportions

# Main indicators
vector_main_indicators <- indicators_components %>% 
  filter(indicator_type %in% c("Energy_Indicators_Unadjusted", "Energy_Indicators_Adjusted",
                               "Emissions_Indicators_Unadjusted", "Emissions_Indicators_Adjusted")) %>% 
  select(-indicator_type)

vector_main_indicators <- as.vector(as.matrix(vector_main_indicators))
vector_main_indicators <- c("land_use_fp", "irrigation_water_use_fp", "erosion_rate_tons_acre", "production",
                            "acres_planted", vector_main_indicators)

one_million <- 1000000

# For scatter plots and trends, with year in X-axis and indicators in Y-axis, simple stuff
shiny_main_indicators <- NIR_calculated %>% 
  select(year, crop, all_of(vector_main_indicators)) %>% 
  rename(
    "Acres Planted" = acres_planted,
    "BTU/acre (adjusted)" = BTUs_per_acre_adjusted,
    "BTU/acre (unadjusted)" = BTUs_per_acre_unadjusted,
    "BTU/yield unit (adjusted)" = BTUs_per_yield_unit_adjusted,
    "BTU/yield unit (unadjusted)" = BTUs_per_yield_unit_unadjusted,
    "CO2e/acre (adjusted)" = CO2e_per_acre_adjusted,
    "CO2e/acre (unadjusted)" = CO2e_per_acre_unadjusted,
    "CO2e/yield unit (adjusted)" = CO2e_per_yield_unit_adjusted,
    "CO2e/yield unit (unadjusted)" = CO2e_per_yield_unit_unadjusted,
    "Soil Erosion (tons/acre)" = erosion_rate_tons_acre,
    "Irrigation Water Use" = irrigation_water_use_fp,
    "Land Use" = land_use_fp,
    "Production (yield units)" = production,
    "Total Energy Use (adjusted)" = total_BTUs_adjusted,
    "Total Energy Use (unadjusted)" = total_BTUs_unadjusted,
    "Total GHG Emissions (adjusted)" = total_CO2e_adjusted,
    "Total GHG Emissions (unadjusted)" = total_CO2e_unadjusted
  ) %>% 
  mutate("Acres Planted (Millions)" = `Acres Planted` / one_million,
         "Production (Million Yield Units)" = `Production (yield units)` / one_million,
         "BTU/Acre (Millions)" = `BTU/acre (adjusted)` / one_million
  )

# Components expressed in per-acre basis
# Filtering out unadjusted variables and the residue removal credit
vector_per_acre <- indicators_components %>% 
  filter(indicator_type %in% c("Energy", "Emissions")) %>% 
  filter(!per_acre %in% c("chemicals_BTUs_per_acre", 
                          "fertilizer_BTUs_per_acre", 
                          "irrigation_CO2e_per_acre",
                          "drying_CO2e_per_acre",
                          "chemicals_CO2e_per_acre", 
                          "fertilizer_CO2e_per_acre",
                          "residue_removal_credit_CO2e_per_acre")) %>% 
  pull(per_acre)

shiny_per_acre <- NIR_calculated %>% 
  select(year, crop, all_of(vector_per_acre))

shiny_per_acre_long <- shiny_per_acre %>% 
  gather(key = "attribute",
         value = "value",
         -c(year, crop)) %>% 
  ungroup() %>% 
  mutate(type = ifelse(str_detect(attribute, "BTU"),
                       "Energy",
                       "GHG")) %>% 
  group_by(year, crop, type) %>% 
  mutate(proportion = value/sum(value)) %>%
  ungroup()

# Components expressed in per-yield-unit basis
vector_per_yield_unit <- indicators_components %>% 
  filter(indicator_type %in% c("Energy", "Emissions")) %>% 
  filter(!per_yield_unit %in% c("chemicals_BTUs_per_yield_unit", 
                                "fertilizer_BTUs_per_yield_unit",
                                "irrigation_CO2e_per_yield_unit",
                                "drying_CO2e_per_yield_unit",
                                "chemicals_CO2e_per_yield_unit",
                                "fertilizer_CO2e_per_yield_unit",
                                "residue_removal_credit_CO2e_per_yield_unit")) %>% 
  pull(per_yield_unit)

shiny_per_yield_unit <- NIR_calculated %>% 
  select(year, crop, all_of(vector_per_yield_unit))

shiny_per_yield_unit_long <- shiny_per_yield_unit %>% 
  gather(key = "attribute",
         value = "value",
         -c(year, crop)) %>% 
  ungroup() %>% 
  mutate(type = ifelse(str_detect(attribute, "BTU"),
                       "Energy",
                       "GHG")) %>% 
  group_by(year, crop, type) %>% 
  mutate(proportion = value/sum(value)) %>%
  ungroup()

# Fertilizers and crop protectants
ag_inputs_tillage <- NIR_calculated %>% 
  mutate(n_rate = fertilizer_nitrogen / acres_planted,
         p_rate = fertilizer_phosphate / acres_planted,
         k_rate = fertilizer_potash / acres_planted,
         herb_rate = chemical_herbicide / acres_planted,
         insect_rate = chemical_insecticide / acres_planted,
         fung_rate = chemical_fungicide / acres_planted,
         fumi_rate = chemical_fumigant / acres_planted,
         growth_rate = chemical_growth_reg / acres_planted) %>% 
  select(year, crop, 
         manure_N_lbs_per_acre, manure_rate_all_acreage,
         conventional_till_share, reduced_till_share, no_till_share,
         water_applied_acre_inches,
         n_rate:growth_rate)

# Exporting final data frame for Shiny app
save(shiny_main_indicators,
     #shiny_per_acre,
     shiny_per_acre_long,
     #shiny_per_yield_unit,
     shiny_per_yield_unit_long,
     ag_inputs_tillage,
     file = "app/outputs_for_ShinyApp_egc.rda")
```

# Exporting Excel files by crop. Allison will distribute the data by request.

```{r}
library(xlsx)

# Exporting main indicators, some components, per yield unit and per acre

my_crops_vector <- sort(unique(NIR_calculated$crop))

excel_variables_to_keep <- readxl::read_excel("data_references/vector_for_data_export.xlsx", sheet = 1)

my_variables_to_keep <-  excel_variables_to_keep$variables_to_keep

my_new_names <- excel_variables_to_keep$new_names

for (k in 1:length(my_crops_vector)) {
  
  df_export <- NIR_calculated %>% 
    filter(crop == my_crops_vector[k]) %>% 
    select(all_of(my_variables_to_keep)) %>%
    mutate_at(.vars = c(3), .funs = ~ round(.x, 6)) %>% 
    mutate_at(.vars = c(4), .funs = ~ round(.x, 4)) %>% 
    mutate_at(.vars = c(5:49), .funs = ~ round(.x, 2))
  
  names(df_export) <- my_new_names
  
  if (my_crops_vector[k] != "Rice") {
    df_export <- df_export %>% 
      select(-c(`Methane GHG Emissions (lb CO2e/acre)`, `Methane GHG Emissions (lb CO2e/yield unit)`))
  }
  
  write.xlsx(as.data.frame(df_export), 
             file = paste0("data_outputs_to_share/", my_crops_vector[k], " 2021 National Indicators Report Data.xlsx"),              sheetName = "Sheet1", col.names = TRUE, row.names = FALSE, append = FALSE)
  
  Sys.sleep(1)
  
}

# verifying file dimensions, rice is different, it has 2 methane variables

my_files <- list.files("data_outputs_to_share/")

for (q in 1:11) {
  
  value <- readxl::read_excel(paste0("data_outputs_to_share/", (my_files[q]))) %>% dim()
  
  print(value)
  
}

```