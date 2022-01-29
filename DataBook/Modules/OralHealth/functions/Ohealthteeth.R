################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ohealthteeth
# AgeRange Sex Valid O2 Stratum PSU WStep1 agerange2 UR Region

ohealthteeth <- function(.data) {

  # variable names that are used in the function
  ohealthteeth_names <- c("sex", "o2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ohealthteeth_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ohealthteeth_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o2>=1 & o2<=6 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o2==1 | o2==2 | o2==3 | o2==4, "1) excellent to average health", NA)) %>% 
      mutate(c = replace(c, o2==5 | o2==6, "2) poor or very poor health")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(o2==1, "1) excellent", NA)) %>% 
      mutate(d = replace(d, o2==2, "2) very good")) %>% 
      mutate(d = replace(d, o2==3, "3) good")) %>% 
      mutate(d = replace(d, o2==4, "4) average")) %>% 
      mutate(d = replace(d, o2==5, "5) poor")) %>% 
      mutate(d = replace(d, o2==6, "6) very poor")) %>% 
      mutate(d = factor(d))
  }

}
