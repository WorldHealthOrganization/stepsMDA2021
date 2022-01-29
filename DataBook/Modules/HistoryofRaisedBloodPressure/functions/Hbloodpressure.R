################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hbloodpressure
# AgeRange Sex Valid H1 H2a H2b H3 PSU Stratum WStep1 agerange2 UR Region

hbloodpressure <- function(.data) {
  
  hbloodpressure_names <- c("sex", "h1", "h2a", "h2b", "h3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hbloodpressure_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hbloodpressure_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(h1cln = if_else(h1==1 | h1==2, 1, 2, missing = 2)) %>% 
      mutate(h2acln = if_else(h1==1 & (h2a==1 | h2a==2), 1, 2, missing = 2)) %>% 
      mutate(h2acln = replace(h2acln, h1==2 & (h2a==2 | is.na(h2a)), 1)) %>% 
      mutate(h2bcln = if_else(h2a==1 & (h2b==1 | h2b==2), 1, 2, missing = 2)) %>% 
      mutate(h2bcln = replace(h2bcln, h2a==2 & (h2b==2 | is.na(h2b)), 1)) %>% 
      mutate(h2bcln = replace(h2bcln, h1==2 & (h2b==2 | is.na(h2b)), 1)) %>% 
      mutate(cln = if_else(h1cln==1 & h2acln==1 & h2bcln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h3cln = if_else(h1==1 & h2a==1 & (h3==1 | h3==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h2b==1, "4) diagnosed within past 12 months", NA)) %>% 
      mutate(c = replace(c, h2a==1 & h2b==2, "3) diagnosed, but not within past 12 months")) %>% 
      mutate(c = replace(c, h2a==2, "2) measured, not diagnosed")) %>% 
      mutate(c = replace(c, h1==2, "1) never measured")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h3==1, "1) taking meds", NA)) %>% 
      mutate(d = replace(d, h3==2, "2) not taking meds")) %>% 
      mutate(d = factor(d))
  }
  
}

