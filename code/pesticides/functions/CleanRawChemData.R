library(tidyverse)


# takes in raw NASS data from 1_get-NASS-data and creates a df with
#  lbsai per ac per pass and total lbs ai 

CleanRawChemData <- function(f.dapps = nass_apps_final,
                             f.dchems = nass_chemicals_final) {
  
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
  
  chem <- 
    chem_app %>% 
    left_join(chem_tot) %>% 
    mutate(domain_category = str_remove_all(domain_category, "\\(|\\)"),
           domain_category = str_remove_all(domain_category, "chemical, ")) %>% 
    separate(domain_category, into = c("class", "ai"), sep = ":") %>% 
    mutate_if(is.character, str_trim, "both")  %>% 
    as_tibble()
  
}
  
