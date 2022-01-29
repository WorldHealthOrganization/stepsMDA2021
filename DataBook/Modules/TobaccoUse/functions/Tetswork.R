################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tetswork
# AgeRange Sex Valid T18 PSU Stratum WStep1 agerange2 UR Region

tetswork <- function(.data) {
  
  # variable names that are used in the function
  tetswork_names <- c("sex", "t18")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tetswork_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tetswork_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((t18==1 | t18==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(t18==1, "exposed at work", NA)) %>% 
      mutate(c = replace(c, t18==2, "not exposed")) %>% 
      mutate(c = factor(c))
  }
  
}

