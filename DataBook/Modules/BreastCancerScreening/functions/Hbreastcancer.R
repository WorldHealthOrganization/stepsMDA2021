################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hbreastcancer
# AgeRange Sex Valid CA1 Stratum PSU WStep1 agerange2 UR Region

hbreastcancer <- function(.data) {

  # variable names that are used in the function
  hbreastcancer_names <- c("sex", "ca1")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hbreastcancer_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hbreastcancer_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((ca1==1 | ca1==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(ca1==1, "1) has been screened", NA)) %>% 
      mutate(c = replace(c, ca1==2, "2) has not been screened")) %>% 
      mutate(c = factor(c))
  }

}
