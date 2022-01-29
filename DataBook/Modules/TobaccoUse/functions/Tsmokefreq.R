################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tsmokefreq
# AgeRange Sex Valid T1 T2 Stratum PSU Wstep1 agerange2 UR Region

tsmokefreq <- function(.data) {
  
  # variable names that are used in the function
  tsmokefreq_names <- c("sex", "t1", "t2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokefreq_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokefreq_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(t1==1 & (t2==1 | t2==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(t1==1 & t2==2, "current smoker (non-daily)", NA)) %>% 
      mutate(c = replace(c, t1==1 & t2==1, "current daily smoker")) %>% 
      mutate(c = factor(c))
  }
  
}
