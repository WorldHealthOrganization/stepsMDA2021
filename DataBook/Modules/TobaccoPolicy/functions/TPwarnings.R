################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in TPwarnings
# AgeRange Sex Valid T1 TP4 PSU Stratum WStep1

tpwarnings <- function(.data) {

  # variable names that are used in the function
  tpwarnings_names <- c("sex", "t1", "tp4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpwarnings_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpwarnings_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(tp4==1, "1) noticed warnings", NA)) %>% 
      mutate(c = replace(c, tp4==2, "2) did not notice warnings")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(t1==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
