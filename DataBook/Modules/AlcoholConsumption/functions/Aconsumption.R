################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Aconsumption
# "agerange", "agerange2", "sex", "valid", "ur", "region",
# "a1", "a2", "a5",
# "psu", "stratum", "wstep1"

aconsumption <- function(.data) {
  
  aconsumption_names <- c("sex", "a1", "a2", "a5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aconsumption_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aconsumption_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2)) %>% 
      mutate(a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2)) %>% 
      mutate(a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1)) %>% 
      mutate(a5cln = if_else(a2==1 & (a5==1 | a5==2), 1, 2, missing = 2)) %>% 
      mutate(a5cln = replace(a5cln, a2==2 & (is.na(a5) | a5==2), 1)) %>% 
      mutate(a5cln = replace(a5cln, a1==2 & (is.na(a5) | a5==2), 1)) %>% 
      mutate(cln = if_else(a1cln==1 & a2cln==1 & a5cln==1 & valid==1, 1, 2, missing = 2)) %>%
      mutate(c = ifelse(a5==1, "1) drank in past 30 days (current drinker)", NA)) %>% 
      mutate(c = replace(c, a2==1 & a5==2, "2) drank in last 12 months, but not current drinker")) %>% 
      mutate(c = replace(c, a1==1 & a2==2, "3) past 12 mos. abstainer")) %>% 
      mutate(c = replace(c, a1==2, "4) lifetime abstainer")) %>% 
      mutate(c = factor(c))
  }
  
}

