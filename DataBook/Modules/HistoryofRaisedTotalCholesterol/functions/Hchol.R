################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hchol
# AgeRange Sex Valid H12 H13a H13b H14 PSU Stratum WStep1 agerange2 UR Region

hchol <- function(.data) {
  
  hchol_names <- c("sex", "h12", "h13a", "h13b", "h14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hchol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hchol_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(h12cln = if_else(h12==1 | h12==2, 1, 2, missing = 2)) %>% 
      mutate(h13acln = if_else(h12==1 & (h13a==1 | h13a==2), 1, 2, missing = 2)) %>% 
      mutate(h13acln = replace(h13acln, h12==2 & (h13a==2 | is.na(h13a)), 1)) %>% 
      mutate(h13bcln = if_else(h13a==1 & (h13b==1 | h13b==2), 1, 2, missing = 2)) %>% 
      mutate(h13bcln = replace(h13bcln, h13a==2 & (h13b==2 | is.na(h13b)), 1)) %>% 
      mutate(h13bcln = replace(h13bcln, h12==2 & (h13b==2 | is.na(h13b)), 1)) %>% 
      mutate(cln = if_else(h12cln==1 & h13acln==1 & h13bcln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h14cln = if_else(h12==1 & h13a==1 & (h14==1 | h14==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h13b==1, "4) diagnosed within past 12 months", NA)) %>% 
      mutate(c = replace(c, h13a==1 & h13b==2, "3) diagnosed, but not within past 12 months")) %>% 
      mutate(c = replace(c, h13a==2, "2) measured, not diagnosed")) %>% 
      mutate(c = replace(c, h12==2, "1) never measured")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h14==1, "1) taking meds", NA)) %>% 
      mutate(d = replace(d, h14==2, "2) not taking meds")) %>% 
      mutate(d = factor(d))
  }
  
}

