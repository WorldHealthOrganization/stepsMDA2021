################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Odentalvisit
# AgeRange Sex Valid O8 Stratum PSU WStep1 agerange2 UR Region

odentalvisit <- function(.data) {

  # variable names that are used in the function
  odentalvisit_names <- c("sex", "o8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, odentalvisit_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(odentalvisit_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o8>=1 & o8<=6 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o8==1 | o8==2, "1) saw dentist in last year", NA)) %>% 
      mutate(c = replace(c, o8==3 | o8==4 | o8==5 | o8==6, "2) has not seen dentist in last year")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else(o8==6, "1) never received dental care", "2) has received dental care", 
                         missing = "2) has received dental care")) %>% 
      mutate(d = factor(d))
  }

}
