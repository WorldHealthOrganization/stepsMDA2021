################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dlower
# AgeRange Sex Valid D9 PSU Stratum WStep1 agerange2 UR Region

dlower <- function(.data) {

  # variable names that are used in the function
  dlower_names <- c("sex", "d9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dlower_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dlower_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(d9==1, "1) very important", NA)) %>% 
      mutate(c = ifelse(d9==2, "2) somewhat important", c)) %>% 
      mutate(c = ifelse(d9==3, "3) not at all important", c)) %>%
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
