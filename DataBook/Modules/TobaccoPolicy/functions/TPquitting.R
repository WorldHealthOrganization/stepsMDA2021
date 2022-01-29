################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in TPquitting
# AgeRange Sex Valid T1 TP4 TP5 PSU Stratum WStep1
# Create a temporary table for analysis

tpquitting <- function(.data) {

  # variable names that are used in the function
  tpquitting_names <- c("sex", "t1", "tp4", "tp5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpquitting_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpquitting_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(tp5==1, "1) thought about quitting", NA)) %>% 
      mutate(c = replace(c, tp5==2, "2) did not think about quitting")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(t1==1 & tp4==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
