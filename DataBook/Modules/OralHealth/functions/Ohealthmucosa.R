################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ohealthmucosa
# AgeRange Sex Valid O4 Stratum PSU WStep1 agerange2 UR Region

ohealthmucosa <- function(.data) {

  # variable names that are used in the function
  ohealthmucosa_names <- c("sex", "o4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ohealthmucosa_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ohealthmucosa_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o4>=1 & o4<=6 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o4==1 | o4==2 | o4==3 | o4==4, "1) excellent to average health", NA)) %>% 
      mutate(c = replace(c, o4==5 | o4==6, "2) poor or very poor health")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(o4==1, "1) excellent", NA)) %>% 
      mutate(d = replace(d, o4==2, "2) very good")) %>% 
      mutate(d = replace(d, o4==3, "3) good")) %>% 
      mutate(d = replace(d, o4==4, "4) average")) %>% 
      mutate(d = replace(d, o4==5, "5) poor")) %>% 
      mutate(d = replace(d, o4==6, "6) very poor")) %>% 
      mutate(d = factor(d))
  }

}
