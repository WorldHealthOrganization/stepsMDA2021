################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hdiabetescontrol
# AgeRange Sex Valid H6 H7a Stratum PSU WStep1 agerange2 UR Region

hdiabetescontrol <- function(.data) {
  
  hdiabetescontrol_names <- c("sex", "h6", "h7a", "hx1", "hx2", "hx3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetescontrol_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetescontrol_names[!i], collapse=", ")))
  } else {
    .data %>% 
      # HX1. Have you received at least 2 HbA1C (glycated haemoglobin) tests in 
      # the past year as part of your diabetes control?
      mutate(hx1cln = if_else(h6==1 & h7a==1 & (hx1==1 | hx1==2) & valid==1, 1, 2, missing = 2)) %>% 
      # HX2. When was the last time your eyes were examined as part of your diabetes control?
      mutate(hx2cln = if_else(h6==1 & h7a==1 & (hx2==1 | hx2==2 | hx2==3) & valid==1, 1, 2, missing = 2)) %>% 
      # HX3. When was the last time your feet were examined as part of your diabetes control?
      mutate(hx3cln = if_else(h6==1 & h7a==1 & (hx3==1 | hx3==2 | hx3==3) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(hx1==1, "1) received at least 2 HbA1C tests", NA)) %>% 
      mutate(c = replace(c, hx1==2, "2) has not received at least 2 HbA1C tests")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(hx2==1, "1) within the past 2 years", NA)) %>% 
      mutate(d = replace(d, hx2==2, "2) more than 2 years ago")) %>% 
      mutate(d = replace(d, hx2==3, "3) never")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(hx3==1, "1) within the last year", NA)) %>% 
      mutate(e = replace(e, hx3==2, "2) more than 1 year ago")) %>% 
      mutate(e = replace(e, hx3==3, "3) never")) %>% 
      mutate(e = factor(e))
  }
  
}

