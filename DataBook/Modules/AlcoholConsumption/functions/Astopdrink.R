################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Astopdrink
# AgeRange Sex Valid A1 A2 A3 PSU Stratum WStep1 agerange2 UR Region

astopdrink <- function(.data) {
  
  astopdrink_names <- c("sex", "a1", "a2", "a3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, astopdrink_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(astopdrink_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2)) %>% 
      mutate(a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2)) %>% 
      mutate(a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1)) %>% 
      mutate(a3cln = if_else((a3==1 | a3==2) & a1==1 & a2==2, 1, 2, missing = 2)) %>% 
      mutate(cln = if_else(a1cln==1 & a2cln==1 & a3cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(a3==1, "1) stopped due to health reasons", NA)) %>% 
      mutate(c = replace(c, a3==2, "2) did not stop due to health reasons")) %>% 
      mutate(c = factor(c))
  }
  
}

