################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in HTSmokefreq
# AgeRange Sex Valid ET1 ET2 PSU Stratum WStep1 agerange2 UR Region

htsmokefreq <- function(.data) {
  
  # variable names that are used in the function
  htsmokefreq_names <- c("sex", "htp1", "htp2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, htsmokefreq_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(htsmokefreq_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(htp1==1 & (htp2==1 | htp2==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(htp1==1 & htp2==2, "2) current user (non-daily)", NA)) %>% 
      mutate(c = replace(c, htp1==1 & htp2==1, "1) current daily user")) %>% 
      mutate(c = factor(c))
  }
  
}
