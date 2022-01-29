################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Anotabletostop
# AgeRange Sex Valid A1 A2 A13 PSU Stratum WStep1 agerange2 UR Region

anotabletostop <- function(.data) {
  
  anotabletostop_names <- c("sex", "a1", "a2", "a13")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, anotabletostop_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(anotabletostop_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(c = ifelse(a13==1 | a13==2 | a13==3, "1) monthly or more frequently",
                        ifelse(a13==4, "2) less than monthly",
                               ifelse(a13==5, "3) never", NA)))) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(a1==1 & a2==1 & a13>=1 & a13<=5 & valid==1, 1, 2, missing = 2))
  }
  
}

