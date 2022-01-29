################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dprocessed
# AgeRange Sex Valid D7 PSU Stratum WStep1 agerange2 UR Region

dprocessed <- function(.data) {

  # variable names that are used in the function
  dprocessed_names <- c("sex", "d7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dprocessed_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dprocessed_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d7==1 | d7==2, "1) always or often consume salty processed food", NA)) %>% 
      mutate(c = ifelse(d7==3 | d7==4 | d7==5, "2) sometimes/rarely/never consume salty processed food", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
