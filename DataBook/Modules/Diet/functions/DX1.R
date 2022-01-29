################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in DX1
# AgeRange Sex Valid DX1 PSU Stratum WStep1 agerange2 UR Region

dx1func <- function(.data) {

  # variable names that are used in the function
  dx1_names <- c("sex", "dx1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dx1_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dx1_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(dx1==1, "1) Once or more a day", NA)) %>% 
      mutate(c = ifelse(dx1==2, "2) 4 to 6 times a week", c)) %>% 
      mutate(c = ifelse(dx1==3, "3) 1 to 3 times a week", c)) %>% 
      mutate(c = ifelse(dx1==4, "4) Less than once a week", c)) %>% 
      mutate(c = ifelse(dx1==5, "5) Never", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else((dx1==1 | dx1==2 | dx1==3 | dx1==4 | dx1==5) & valid==1, 1, 2, missing = 2))
  }

}
