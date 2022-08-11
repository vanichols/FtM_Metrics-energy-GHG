
library(tidyverse)
library(readxl)
library(usdarnass)

#--what are the most recent years of data for a given crop and class?

GetMostRecentTwoYears <- function(f.dat = chem,
                                  f.comm = "corn",
                                  f.class = "herbicide") {
  chem %>% 
    ungroup() %>% 
    filter(crop == f.comm,
           class == f.class) %>% 
    select(year) %>% 
    distinct() %>% 
    arrange(-year) %>% 
    slice(1:2) %>% 
    pull(year)
  
}

