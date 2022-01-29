################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hcvd
# AgeRange Sex Valid H17 Stratum PSU WStep1 agerange2 UR Region

hcvd <- function(.data) {

  # variable names that are used in the function
  hcvd_names <- c("sex", "h17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcvd_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcvd_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(h17cln = if_else((h17==1 | h17==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h17==1, "1) had heart attack/chest pain", NA)) %>% 
      mutate(c = replace(c, h17==2, "2) did not have heart attack/chest pain")) %>% 
      mutate(c = factor(c))
  }

}
