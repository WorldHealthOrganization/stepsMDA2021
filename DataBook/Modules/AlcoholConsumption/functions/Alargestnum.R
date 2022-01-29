################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Alargestnum
# AgeRange Sex Valid A1 A2 A5 A8 PSU Stratum WStep1 agerange2 UR Region

alargestnum <- function(.data) {

  # variable names that are used in the function
  alargestnum_names <- c("sex", "a1", "a2", "a5", "a8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, alargestnum_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(alargestnum_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(a1==1 & a2==1 & a5==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(a8) | a8==0 | a8>50, 2))
  }

}
