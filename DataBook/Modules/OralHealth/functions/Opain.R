################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Opain
# AgeRange Sex Valid O7 Stratum PSU WStep1 agerange2 UR Region

opain <- function(.data) {

  # variable names that are used in the function
  opain_names <- c("sex", "o7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, opain_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(opain_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((o7==1 | o7==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o7==1, "1) experienced pain", NA)) %>% 
      mutate(c = replace(c, o7==2, "2) did not experience pain")) %>% 
      mutate(c = factor(c))
  }

}
