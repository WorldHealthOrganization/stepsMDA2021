################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Cworkpaid
# AgeRange Sex Valid C8 UR Region agerange2

cworkpaid <- function(.data) {

  # variable names that are used in the function
  cworkpaid_names <- c("sex", "c8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cworkpaid_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cworkpaid_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(c8==1, "1) Government employee", NA)) %>% 
      mutate(c = replace(c, c8==2, "2) Non-government employee")) %>% 
      mutate(c = replace(c, c8==3, "3) Self-employed")) %>% 
      mutate(c = replace(c, c8==4 | c8==5 | c8==6 | c8==7 | c8==8 | c8==9, "4) Unpaid")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1))
  }

}
