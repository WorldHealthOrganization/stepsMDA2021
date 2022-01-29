################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tcessation
# AgeRange Sex Valid T1 T2 T6 T7 PSU Stratum WStep1 agerange2 UR Region

tcessation <- function(.data) {
  
  # variable names that are used in the function
  tcessation_names <- c("sex", "t1", "t2", "t6", "t7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tcessation_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tcessation_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = 2) %>% 
      mutate(stopcln = 2) %>% 
      mutate(mdcln = 2) %>% 
      mutate(cln = replace(cln, t1==1 & (t2==1 | t2==2) & valid==1, 1)) %>% 
      mutate(stopcln = replace(stopcln, (t6==1 | t6==2) & cln==1, 1)) %>% 
      mutate(mdcln = replace(mdcln, (t7==1 | t7==2) & cln==1, 1)) %>% 
      mutate(c = if_else(t6==1, "1) tried to stop smoking", "2) didn't try to stop smoking", 
                         missing = "2) didn't try to stop smoking")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else(t7==1, "1) advised to quit", "2) not advised to quit", 
                         missing = "2) not advised to quit")) %>% 
      mutate(d = factor(d)) 
  }
  
}

