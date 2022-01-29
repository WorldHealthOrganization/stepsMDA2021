################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Deating
# AgeRange Sex Valid D5 PSU Stratum WStep1 agerange2 UR Region

deating <- function(.data) {

  # variable names that are used in the function
  deating_names <- c("sex", "d5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, deating_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(deating_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d5==1 | d5==2, "1) always or often added salt", NA)) %>% 
      mutate(c = ifelse(d5==3 | d5==4 | d5==5, "2) sometimes/rarely/never added salt", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
