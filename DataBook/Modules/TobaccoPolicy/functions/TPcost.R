################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Note from EpiInfo: 
# "This program assumes a maximum of 7776 (error code of 7777) for the number of cigarettes 
# purchased and the amount spent on this purchase. If your data uses more or less, 
# please modify the program accordingly or ask the STEPS team for assistance."

# Variables used in TPcost
# AgeRange Sex Valid T1 T2 T5a T5aw TP6 TP7 PSU Stratum WStep1

tpcost <- function(.data) {

  # variable names that are used in the function
  tpcost_names <- c("sex", "t1", "t2", "t5a", "t5aw", "tp6", "tp7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpcost_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpcost_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(tp6cln = if_else(tp6>0 & tp6<7777, 1, 2, missing = 2)) %>% 
      mutate(tp7cln = if_else(tp7>0 & tp7<7777, 1, 2, missing = 2)) %>% 
      mutate(priceper20cigs = (tp7 / tp6) * 20) %>% 
      mutate(cln = if_else(t1==1 & tp6cln==1 & tp7cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cigcln = cln) %>% 
      mutate(cigcln = replace(cigcln, t2==1 & (is.na(t5a) | t5a>50), 2)) %>% 
      mutate(cigcln = replace(cigcln, t2==2 & (is.na(t5aw) | t5aw>350), 2)) %>% 
      mutate(cigcln = replace(cigcln, t5a>50 | t5aw>350, 2)) %>% 
      mutate(cigspermonth = ifelse(t1==1 & t5aw>0 & t5aw<=350, t5aw * 4, NA)) %>% 
      mutate(cigspermonth = ifelse(t2==1 & t5a>0 & t5a<=50, t5a * 30, cigspermonth)) %>% 
      mutate(monthlyexp = cigspermonth * (tp7 / tp6))
  }

}
