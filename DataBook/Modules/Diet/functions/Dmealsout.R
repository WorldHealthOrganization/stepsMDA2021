################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dmealsout
# AgeRange Sex Valid D13 PSU WStep1 Stratum agerange2 UR Region

dmealsout <- function(.data) {

  # variable names that are used in the function
  dmealsout_names <- c("sex", "d13")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dmealsout_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dmealsout_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(d13>=0 & d13<=30 & valid==1, 1, 2, missing = 2))
  }
}
