################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Mhip
# AgeRange Sex Valid M15 M8 PSU Stratum WStep2 agerange2 UR Region

mhip <- function(.data) {

  # variable names that are used in the function
  mhip_names <- c("sex", "m8", "m15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mhip_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mhip_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(m15cln = if_else(m15>=45 & m15<=300 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(m15cln = replace(m15cln, sex=="Women" & m8==1, 2))
  }

}
