################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tcig
# AgeRange Sex Valid T1 T2 T5a T5b PSU Stratum WStep1 agerange2 UR Region

tcig <- function(.data) {
  
  # variable names that are used in the function
  tcig_names <- c("sex", "t1", "t2", "t5a", "t5b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tcig_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tcig_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(t1==1 & t2==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, t5a>50 | is.na(t5a) | t5b>50 | is.na(t5b), 2)) %>% 
      mutate(total = t5a + t5b) %>% 
      mutate(c = ifelse(total>=0 & total<=4, "1) <5 cigs", NA)) %>% 
      mutate(c = replace(c, total>=5 & total<=9, "2) 5-9 cigs")) %>% 
      mutate(c = replace(c, total>=10 & total<=14, "3) 10-14 cigs")) %>% 
      mutate(c = replace(c, total>=15 & total<=24, "4) 15-24 cigs")) %>% 
      mutate(c = replace(c, total>=25 & total<=100, "5) >= 25 cigs")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = replace(cln, total==0, 2))
  }
  
}

