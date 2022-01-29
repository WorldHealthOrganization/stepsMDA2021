################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Afamproblem
# AgeRange Sex Valid A16 PSU Stratum WStep1 agerange2 UR Region

afamproblem <- function(.data) {
  
  afamproblem_names <- c("sex", "a16")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afamproblem_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afamproblem_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(c = ifelse(a16==1 | a16==2, "1) monthly or more frequently",
                        ifelse(a16==3 | a16==4, "2) less than monthly",
                               ifelse(a16==5, "3) never", NA)))) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(a16>=1 & a16<=5 & valid==1, 1, 2, missing = 2))
  }
  
}

