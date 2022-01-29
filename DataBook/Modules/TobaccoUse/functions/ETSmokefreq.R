################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in ETSmokefreq
# AgeRange Sex Valid ET1 ET2 PSU Stratum WStep1 agerange2 UR Region

etsmokefreq <- function(.data) {
  
  # variable names that are used in the function
  etsmokefreq_names <- c("sex", "et1", "et2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, etsmokefreq_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(etsmokefreq_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(et1==1 & (et2==1 | et2==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(et1==1 & et2==2, "2) current user (non-daily)", NA)) %>% 
      mutate(c = replace(c, et1==1 & et2==1, "1) current daily user")) %>% 
      mutate(c = factor(c))
  }
  
}
