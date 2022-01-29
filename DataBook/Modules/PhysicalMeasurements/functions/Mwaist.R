################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Mwaist
# AgeRange Sex Valid M14 M8 PSU Stratum WStep2 agerange2 UR Region

mwaist <- function(.data) {

  # variable names that are used in the function
  mwaist_names <- c("sex", "m8", "m14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mwaist_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mwaist_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(m14cln = if_else(m14>=30 & m14<=200 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(m14cln = replace(m14cln, sex=="Women" & m8==1, 2))
  }

}
