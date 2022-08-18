library(tidyverse)


# takes in raw NASS data from 1_get-NASS-data and creates a df with
#  lbsai per ac per pass and total lbs ai 

CleanRawChemData <- function(f.dapps = nass_apps_final,
                             f.dchems = nass_chemicals_final) {

  pest_codes <- read_csv("data_references/pesticide_codes.csv")
    
  #--remove fertilizer applications, combine amount per application with total amounts
  chem_app <- 
    f.dapps %>% 
    filter(domain != "fertilizer") %>% 
    rename("lbsai_ac_app" = value) %>% 
    select(-data_item, -domain)
  
  chem_tot <- 
    f.dchems %>% 
    filter(domain != "fertilizer") %>% 
    rename("lbsai" = value) %>% 
    select(-data_item, -domain)
  
  chem_raw <- 
    chem_app %>% 
    left_join(chem_tot) %>% 
    mutate(domain_category = str_remove_all(domain_category, "\\(|\\)"),
           domain_category = str_remove_all(domain_category, "chemical, ")) %>% 
    separate(domain_category, into = c("class", "ai"), sep = ":") %>% 
    mutate_if(is.character, str_trim, "both")  %>%
    #--make simplified class, it currently has 8 categories
    mutate(class_full = class,
           class = case_when(
             grepl("fungicide", class) ~ "fungicide",
             grepl("herbicide", class) ~ "herbicide",
             grepl("insecticide", class) ~ "insecticide",
             grepl("other", class) ~ "other",
           ))
  
  #--try to get the class of the 'other' category
  chem_other <- 
    chem_raw %>% 
    filter(class == "other") %>% 
    separate(ai, into = c("ai_chem", "ai_code"), sep = "=", remove = F) %>% 
    mutate_if(is.character, str_trim, "both") %>% 
    mutate(ai_code = as.numeric(ai_code)) %>% 
    left_join(pest_codes %>% 
                rename(ai_code = code)) %>% 
    mutate(class = ifelse(is.na(type), "other", type)) %>% 
    select(year, crop, class, ai, lbsai_ac_app, lbsai) %>% 
    mutate_if(is.character, str_to_lower)
  
  chem <- 
    chem_raw %>% 
    select(-class_full) %>% 
    #--remove the old others, replace with newly assigned others
    filter(!class == "other") %>% 
    bind_rows(chem_other) %>% 
    filter(class %in% 
             c("fungicide", "herbicide", "insecticide", "growth reg", "fumigant"))
  
  
}
 
