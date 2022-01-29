################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Onatural
# AgeRange Sex Valid O1 Stratum PSU WStep1 agerange2 UR Region

onatural <- function(.data) {

  # variable names that are used in the function
  onatural_names <- c("sex", "o1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, onatural_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(onatural_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o1>=1 & o1<=4 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o1==1, "1) no natural teeth", NA)) %>% 
      mutate(c = replace(c, o1==2, "2) 1 to 9 teeth")) %>% 
      mutate(c = replace(c, o1==3, "3) 10 to 19 teeth")) %>% 
      mutate(c = replace(c, o1==4, "4) 20 teeth or more")) %>% 
      mutate(c = factor(c))
  }

}
