################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tetshome
# AgeRange Sex Valid T17 PSU Stratum WStep1 agerange2 UR Region

tetshome <- function(.data) {
  
  # variable names that are used in the function
  tetshome_names <- c("sex", "t17")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tetshome_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tetshome_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((t17==1 | t17==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(t17==1, "exposed at home", NA)) %>% 
      mutate(c = replace(c, t17==2, "not exposed")) %>% 
      mutate(c = factor(c))
  }
  
}

