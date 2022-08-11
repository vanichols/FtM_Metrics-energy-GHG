#--note this is to make terminology match the ref_ai_energy document, which is derived from table 2 of audsely et al. 2009


LumpAis <- function(x) {
  x %>% 
    mutate(ai2 = 
           case_when(
             grepl("glyphosate", ai) ~ "glyphosate",
             grepl("2,4-d", ai) ~ "2,4-d",
             grepl("dicamba", ai) ~ "dicamba",
             grepl("metolachlor", ai) ~ "metolachlor",
             grepl("glufosinate", ai) ~ "glufosinate",
             grepl("mcpa", ai) ~ "mcpa",
             grepl("bromoxynil octanoate", ai) ~ "bromoxynil octanoate",
             grepl("paraquat", ai) ~ "paraquat",
             grepl("trifluralin", ai) ~ "trifluralin",
             grepl("diuron", ai) ~ "diuron",
             grepl("pendimethalin", ai) ~ "pendimethalin",
             grepl("metribuzin", ai) ~ "metribuzin",
             grepl("dimethenamid-p", ai) ~ "dimethenamid-p",
             grepl("eptc", ai) ~ "eptc",
             grepl("ethalfluralin", ai) ~ "ethalfluralin",
             grepl("propanil", ai) ~ "propanil",
             grepl("thiobencarb", ai) ~ "thiobencarb",
             grepl("clomazone", ai) ~ "clomazone",
             grepl("fluroxypyr", ai) ~ "fluroxypyr",
             grepl("desmedipham", ai) ~ "desmedipham",
             grepl("phenmedipham", ai) ~ "phenmedipham",
             grepl("cycloate", ai) ~ "cycloate",
             grepl("clopyralid", ai) ~ "clopyralid",
             grepl("clethodim", ai) ~ "clethodim",
             grepl("ethofumesate", ai) ~ "ethofumesate",
             
             
             
             
             
             
             
             TRUE ~ ai
           )
    )
  
}
