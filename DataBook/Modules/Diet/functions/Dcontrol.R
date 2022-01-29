################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dcontrol
# AgeRange Sex Valid D11a D11b D11c D11d D11e D11f PSU Stratum WStep1 agerange2 UR Region

dcontrol <- function(.data) {

  # variable names that are used in the function
  dcontrol_names <- c("sex", "d11a", "d11b", "d11c", "d11d", "d11e", "d11f")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dcontrol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dcontrol_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(a = ifelse(d11a==1, "1) limit consumption of processed foods", NA)) %>% 
      mutate(a = ifelse(d11a==2, "2) did not limit consumption of processed foods", a)) %>% 
      mutate(a = factor(a)) %>% 
      mutate(b = ifelse(d11b==1, "1) looked at salt labels on food", NA)) %>% 
      mutate(b = ifelse(d11b==2, "2) did not look at salt labels on food", b)) %>% 
      mutate(b = factor(b)) %>% 
      mutate(c = ifelse(d11c==1, "1) bought low salt alternatives", NA)) %>% 
      mutate(c = ifelse(d11c==2, "2) did not buy low salt alternatives", c)) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(d11d==1, "1) used other spices when cooking", NA)) %>% 
      mutate(d = ifelse(d11d==2, "2) did not use other spices when cooking", d)) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(d11e==1, "1) avoided eating out", NA)) %>% 
      mutate(e = ifelse(d11e==2, "2) did not avoid eating out", e)) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(d11f==1, "1) did some other behavior", NA)) %>% 
      mutate(f = ifelse(d11f==2, "2) did not do some other behavior", f)) %>% 
      mutate(f = factor(f)) %>% 
      ###
      mutate(d11acln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11acln = replace(d11acln, is.na(a), 2)) %>% 
      mutate(d11bcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11bcln = replace(d11bcln, is.na(b), 2)) %>% 
      mutate(d11ccln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11ccln = replace(d11ccln, is.na(c), 2)) %>% 
      mutate(d11dcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11dcln = replace(d11dcln, is.na(d), 2)) %>% 
      mutate(d11ecln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11ecln = replace(d11ecln, is.na(e), 2)) %>% 
      mutate(d11fcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(d11fcln = replace(d11fcln, is.na(f), 2)) 
  }

}
