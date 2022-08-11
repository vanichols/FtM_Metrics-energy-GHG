
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
             TRUE ~ ai
           )
    )
  
}
