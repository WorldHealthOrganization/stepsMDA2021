################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Amorningdrink
# AgeRange Sex Valid A1 A2 A15 PSU Stratum WStep1 agerange2 UR Region

amorningdrink <- function(.data) {
  
  amorningdrink_names <- c("sex", "a1", "a2", "a15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, amorningdrink_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(amorningdrink_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(c = ifelse(a15==1 | a15==2 | a15==3, "1) monthly or more frequently",
                        ifelse(a15==4, "2) less than monthly",
                               ifelse(a15==5, "3) never", NA)))) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(a1==1 & a2==1 & a15>=1 & a15<=5 & valid==1, 1, 2, missing = 2))
  }
  
}

