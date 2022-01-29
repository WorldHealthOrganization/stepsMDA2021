################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ohealthgums
# AgeRange Sex Valid O3 Stratum PSU WStep1 agerange2 UR Region

ohealthgums <- function(.data) {

  # variable names that are used in the function
  ohealthgums_names <- c("sex", "o3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ohealthgums_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ohealthgums_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o3>=1 & o3<=6 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o3==1 | o3==2 | o3==3 | o3==4, "1) excellent to average health", NA)) %>% 
      mutate(c = replace(c, o3==5 | o3==6, "2) poor or very poor health")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(o3==1, "1) excellent", NA)) %>% 
      mutate(d = replace(d, o3==2, "2) very good")) %>% 
      mutate(d = replace(d, o3==3, "3) good")) %>% 
      mutate(d = replace(d, o3==4, "4) average")) %>% 
      mutate(d = replace(d, o3==5, "5) poor")) %>% 
      mutate(d = replace(d, o3==6, "6) very poor")) %>% 
      mutate(d = factor(d))
  }

}
