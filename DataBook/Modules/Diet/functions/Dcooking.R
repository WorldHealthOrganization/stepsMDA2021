################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dcooking
# AgeRange Sex Valid D6 PSU Stratum WStep1 agerange2 UR Region

dcooking <- function(.data) {

  # variable names that are used in the function
  dcooking_names <- c("sex", "d6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dcooking_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dcooking_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d6==1 | d6==2, "1) always or often added salt", NA)) %>% 
      mutate(c = ifelse(d6==3 | d6==4 | d6==5, "2) sometimes/rarely/never added salt", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
