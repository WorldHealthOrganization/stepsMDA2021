################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Bhdlipids
# AgeRange Sex Valid B17 PSU Stratum WStep3 agerange2 UR Region

bhdlipids <- function(.data) {

  # variable names that are used in the function
  bhdlipids_names <- c("sex", "b17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, bhdlipids_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(bhdlipids_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(b17>=0.1 & b17<=5 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(b17<1.03, "HDL <1.03mmol/L", "HDL >=1.03 mmol/L")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(b17<1.29, "HDL <1.29mmol/L", "HDL >=1.29 mmol/L")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(b17mg = b17*38.67)
  }

}
