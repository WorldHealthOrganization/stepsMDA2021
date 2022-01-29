################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in TPcigads
# AgeRange Sex Valid TP2 PSU Stratum WStep1

tpcigads <- function(.data) {

  # variable names that are used in the function
  tpcigads_names <- c("sex", "tp2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpcigads_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpcigads_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(tp2==1, "1) noticed information", NA)) %>% 
      mutate(c = replace(c, tp2==2, "2) did not notice information")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2)) 
  }

}
