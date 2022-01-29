################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Afailexpected
# AgeRange Sex Valid A1 A2 A14 PSU Stratum WStep1 agerange2 UR Region

afailexpected <- function(.data) {
  
  afailexpected_names <- c("sex", "a1", "a2", "a14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afailexpected_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afailexpected_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(c = ifelse(a14==1 | a14==2 | a14==3, "1) monthly or more frequently",
                        ifelse(a14==4, "2) less than monthly",
                               ifelse(a14==5, "3) never", NA)))) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(a1==1 & a2==1 & a14>=1 & a14<=5 & valid==1, 1, 2, missing = 2))
  }
  
}
