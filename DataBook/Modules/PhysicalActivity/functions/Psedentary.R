################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Psedentary & Psedentarymedian
# AgeRange Sex Valid P16a P16b WStep1 Stratum PSU agerange2 UR Region

psedentary <- function(.data) {

  # variable names that are used in the function
  psedentary_names <- c("sex")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, psedentary_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(psedentary_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(p16cln==1 & valid==1, 1, 2, missing = 2))
  }

}
