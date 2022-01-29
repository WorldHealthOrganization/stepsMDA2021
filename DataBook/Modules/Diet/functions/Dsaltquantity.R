################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dsaltquantity
# AgeRange Sex Valid D8 PSU Stratum WStep1 agerange2 UR Region

dsaltquantity <- function(.data) {

  # variable names that are used in the function
  dsaltquantity_names <- c("sex", "d8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dsaltquantity_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dsaltquantity_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d8==1 | d8==2, "1) eat far too much or too much salt", NA)) %>% 
      mutate(c = ifelse(d8==3 | d8==4 | d8==5, "2) eat right amount or less salt", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(d8==1, "1) eat far too much salt", NA)) %>% 
      mutate(d = ifelse(d8==2, "2) eat too much salt", d)) %>% 
      mutate(d = ifelse(d8==3, "3) eat right amount of salt", d)) %>%
      mutate(d = ifelse(d8==4, "4) eat too little salt", d)) %>%
      mutate(d = ifelse(d8==5, "5) eat far too little salt", d)) %>%
      mutate(d = factor(d)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
