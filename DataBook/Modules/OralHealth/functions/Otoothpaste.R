################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Otoothpaste
# AgeRange Sex Valid O11 Stratum PSU WStep1 agerange2 UR Region

otoothpaste <- function(.data) {

  # variable names that are used in the function
  otoothpaste_names <- c("sex", "o11")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, otoothpaste_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(otoothpaste_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o11>=1 & o11<=2 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o11==1, "1) uses toothpaste", NA)) %>% 
      mutate(c = replace(c, o11==2, "2) does not use toothpaste")) %>% 
      mutate(c = factor(c))
  }

}
