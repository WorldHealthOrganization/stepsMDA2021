################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hbreastcancertime
# AgeRange Sex Valid CA1 CA3a Stratum PSU WStep1 agerange2 UR Region

hbreastcancertime <- function(.data) {

  # variable names that are used in the function
  hbreastcancertime_names <- c("sex", "ca1", "ca3a")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hbreastcancertime_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hbreastcancertime_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((ca3a==1 | ca3a==2 | ca3a==3 | ca3a==4 | ca3a==5) & ca1==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(ca3a==1, "1) In the last 12 months", NA)) %>% 
      mutate(c = ifelse(ca3a==2, "2) More than 1, less than 2 years ago", c)) %>% 
      mutate(c = ifelse(ca3a==3, "3) More than 2, less than 5 years ago", c)) %>% 
      mutate(c = ifelse(ca3a==4, "4) More than 5 years ago", c)) %>% 
      mutate(c = ifelse(ca3a==5, "5) Never", c)) %>% 
      mutate(c = factor(c))
  }

}
