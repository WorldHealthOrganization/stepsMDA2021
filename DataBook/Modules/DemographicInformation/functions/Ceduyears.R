################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ceduyears
# AgeRange Sex Valid C4 agerange2 UR Region

ceduyears <- function(.data) {

  # variable names that are used in the function
  ceduyears_names <- c("sex", "c4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ceduyears_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ceduyears_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(is.na(c4) | c4>30 | valid==2, 2, 1, missing = 1)) 
  }

}
