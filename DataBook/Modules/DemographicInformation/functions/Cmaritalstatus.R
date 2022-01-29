################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Cmaritalstatus
# AgeRange Sex Valid C7 UR Region agerange2

cmaritalstatus <- function(.data) {

  # variable names that are used in the function
  cmaritalstatus_names <- c("sex", "c7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cmaritalstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cmaritalstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(c7==1, "1) Never Married", NA)) %>% 
      mutate(c = replace(c, c7==2, "2) Currently married")) %>% 
      mutate(c = replace(c, c7==3, "3) Separated")) %>% 
      mutate(c = replace(c, c7==4, "4) Divorced")) %>% 
      mutate(c = replace(c, c7==5, "5) Widowed")) %>% 
      mutate(c = replace(c, c7==6, "6) Cohabitating")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1))
  }

}
