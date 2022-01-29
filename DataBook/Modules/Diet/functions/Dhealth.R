################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dhealth
# AgeRange Sex Valid D10 PSU Stratum WStep1 agerange2 UR Region

dhealth <- function(.data) {

  # variable names that are used in the function
  dhealth_names <- c("sex", "d10")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dhealth_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dhealth_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d10==1, "1) think too much salt can cause health problems", NA)) %>% 
      mutate(c = ifelse(d10==2, "2) don't think too much salt can cause health problems", c)) %>% 
      mutate(c = ifelse(d10==77, "3) don't know if salt can cause health problems", c)) %>%
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
