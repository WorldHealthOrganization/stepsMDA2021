################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Bglucose
# AgeRange Sex Valid H6 H7a H8 H9 B1 B5 B6 PSU Stratum WStep3 agerange2 UR Region

bglucose <- function(.data) {

  # variable names that are used in the function
  bglucose_names <- c("sex", "h6", "h7a", "h8", "h9", "b1", "b5", "b6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, bglucose_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(bglucose_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(fasted = if_else(b1==2, 1, 2, missing = 2)) %>% 
      mutate(b5cln = if_else(b5>=1 & b5<=35, 1, 2, missing = 2)) %>% 
      mutate(cln = if_else(fasted==1 & b5cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(clnall = if_else(((fasted==1 & b5cln==1) | b6==1) & valid==1, 1, 2, missing = 2)) %>% 
      ###
      mutate(medstext = ifelse(h8==1 | h9==1 | b6==1, "1) currently on treatment for raised blood glucose", NA)) %>% 
      mutate(medstext = replace(medstext, (h6==2 | h7a==2 | (h8==2 & h9==2)) & b6==2, "2) not currently on treatment for raised blood glucose")) %>% 
      mutate(medstext = replace(medstext, h6==2 & is.na(b5) & is.na(b6), "2) not currently on treatment for raised blood glucose")) %>% 
      mutate(medstext = replace(medstext, is.na(medstext), "3) incomplete information regarding treatment for raised blood glucose")) %>% 
      mutate(medstext = factor(medstext)) %>% 
      ###
      mutate(d = ifelse(b5<6.1, "1) blood glucose <6.1", NA)) %>% 
      mutate(d = replace(d, b5>=6.1 & b5<7.0, "2) blood glucose >=6.1 AND <7.0")) %>% 
      mutate(d = replace(d, b5>=7.0 | b6==1, "3) blood glucose >=7.0 or took meds today")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(b5mg = b5*18.01) %>% 
      ###
      mutate(diabtr = if_else(h8==1 | h9==1 | b6==1, 1, 2, missing = 2)) %>% 
      mutate(diagn = ifelse(b5>=7.0 & b5<=35 & (h7a==2 | is.na(h7a)), "1)raised blood glucose, not previously diagnosed", NA)) %>% 
      mutate(diagn = replace(diagn, h7a==1 & diabtr==2, "2) diagnosed but not on medication")) %>% 
      mutate(diagn = replace(diagn, h7a==1 & diabtr==1, "3) diagnosed and on medication")) %>% 
      mutate(diagn = replace(diagn, (h7a==2 | is.na(h7a)) & b5<7.0 & b5>=1, "4) BG not raised and not diagnosed")) %>% 
      mutate(diagn = factor(diagn))
  }

}
