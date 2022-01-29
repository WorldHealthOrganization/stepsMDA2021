################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ofreqclean
# AgeRange Sex Valid O10 Stratum PSU WStep1 agerange2 UR Region

ofreqclean <- function(.data) {

  # variable names that are used in the function
  ofreqclean_names <- c("sex", "o10")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ofreqclean_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ofreqclean_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o10>=1 & o10<=7 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o10==1 | o10==2 | o10==3 | o10==4 | o10==5, "2) less than daily", NA)) %>% 
      mutate(c = replace(c, o10==6 | o10==7, "1) daily")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(o10==1 | o10==2 | o10==3 | o10==4 | o10==5 | o10==6, "2) less than twice daily", NA)) %>% 
      mutate(d = replace(d, o10==7, "1) twice daily")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(o10==1, "1) never", NA)) %>% 
      mutate(e = replace(e, o10==2, "2) once a month")) %>% 
      mutate(e = replace(e, o10==3, "3) 2-3 times a month")) %>% 
      mutate(e = replace(e, o10==4, "4) once a week")) %>% 
      mutate(e = replace(e, o10==5, "5) 2-6 times a week")) %>% 
      mutate(e = replace(e, o10==6, "6) once a day")) %>% 
      mutate(e = replace(e, o10==7, "7) twice a day or more")) %>% 
      mutate(e = factor(e))
  }

}
