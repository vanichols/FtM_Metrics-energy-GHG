library(tidyverse)
library(readxl)


# takes in raw NASS data from 1_get-NASS-data and creates a df with
#  lbsai per ac per pass and total lbs ai 

CleanRawChemData <- function(f.dapps = nass_apps,
                             f.dchems = nass_chemicals) {
  
  #--remove fertilizer applications, combine amount per application with total amounts
  chem_app <- 
    f.dapps %>% 
    filter(Domain != "FERTILIZER") %>% 
    janitor::clean_names() %>% 
    mutate_if(is.character, str_to_lower) %>% 
    rename("lbsai_ac_app" = value) %>% 
    select(-data_item, -domain)
  
  chem_tot <- 
    nass_chemicals %>% 
    filter(Domain != "FERTILIZER") %>% 
    janitor::clean_names() %>% 
    mutate_if(is.character, str_to_lower) %>% 
    rename("lbsai" = value) %>% 
    select(-data_item, -domain)
  
  chem <- 
    chem_app %>% 
    left_join(chem_tot) %>% 
    filter(!is.na(lbsai_ac_app)) %>% 
    mutate(domain_category = str_remove_all(domain_category, "\\(|\\)"),
           domain_category = str_remove_all(domain_category, "chemical, ")) %>% 
    separate(domain_category, into = c("class", "ai"), sep = ":") %>% 
    mutate_if(is.character, str_trim, "both")  %>% 
    as_tibble()
  
}
  
