################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hdiabetes
# AgeRange Sex Valid H6 H7a H7b H8 H9 PSU Stratum WStep1 agerange2 UR Region

hdiabetes <- function(.data) {
  
  hdiabetes_names <- c("sex", "h6", "h7a", "h7b", "h8", "h9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetes_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetes_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(h6cln = if_else(h6==1 | h6==2, 1, 2, missing = 2)) %>% 
      mutate(h7acln = if_else(h6==1 & (h7a==1 | h7a==2), 1, 2, missing = 2)) %>% 
      mutate(h7acln = replace(h7acln, h6==2 & (h7a==2 | is.na(h7a)), 1)) %>% 
      mutate(h7bcln = if_else(h7a==1 & (h7b==1 | h7b==2), 1, 2, missing = 2)) %>% 
      mutate(h7bcln = replace(h7bcln, h7a==2 & (h7b==2 | is.na(h7b)), 1)) %>% 
      mutate(h7bcln = replace(h7bcln, h6==2 & (h7b==2 | is.na(h7b)), 1)) %>% 
      mutate(cln = if_else(h6cln==1 & h7acln==1 & h7bcln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h7b==1, "4) diagnosed within past 12 months", NA)) %>% 
      mutate(c = replace(c, h7a==1 & h7b==2, "3) diagnosed, but not within past 12 months")) %>% 
      mutate(c = replace(c, h7a==2, "2) measured, not diagnosed")) %>% 
      mutate(c = replace(c, h6==2, "1) never measured")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(h8cln = if_else(h6==1 & h7a==1 & (h8==1 | h8==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h9cln = if_else(h6==1 & h7a==1 & (h9==1 | h9==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(d = ifelse(h8==1, "1) taking meds", NA)) %>% 
      mutate(d = replace(d, h8==2, "2) not taking meds")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(h9==1, "1) taking insulin", NA)) %>% 
      mutate(e = replace(e, h9==2, "2) not taking insulin")) %>% 
      mutate(e = factor(e))
  }
  
}

