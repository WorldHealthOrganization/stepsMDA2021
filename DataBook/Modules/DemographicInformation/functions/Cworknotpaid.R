################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Cworknotpaid
# AgeRange Sex Valid C8 UR Region agerange2

cworknotpaid <- function(.data) {

  # variable names that are used in the function
  cworknotpaid_names <- c("sex", "c8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cworknotpaid_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cworknotpaid_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(c8==4, "1) Non-paid", NA)) %>% 
      mutate(c = replace(c, c8==5, "2) Student")) %>% 
      mutate(c = replace(c, c8==6, "3) Homemaker")) %>% 
      mutate(c = replace(c, c8==7, "4) Retired")) %>% 
      mutate(c = replace(c, c8==8, "5) Unemployed able to work")) %>% 
      mutate(c = replace(c, c8==9, "6) Unemployed unable to work")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1))
  }

}
