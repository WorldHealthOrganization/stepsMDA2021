################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ceduhigh
# AgeRange Sex Valid C5 UR Region agerange2

ceduhigh <- function(.data) {

  # variable names that are used in the function
  ceduhigh_names <- c("sex", "c5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ceduhigh_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ceduhigh_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(c5==1, "1) No formal schooling", NA)) %>% 
      mutate(c = replace(c, c5==2, "2) Less than primary school (1-4 form)")) %>% 
      mutate(c = replace(c, c5==3, "3) Primary school completed (5-9 form)")) %>% 
      mutate(c = replace(c, c5==4, "4) Secondary school completed (10-11 form, lyceum)")) %>% 
      mutate(c = replace(c, c5==5, "5) Secondary professional (college)")) %>% 
      mutate(c = replace(c, c5==6, "6) University degree (bachelor) / Associate of Science")) %>% 
      mutate(c = replace(c, c5==7, "7) Post graduate degree (Masters degree, PhD)")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(is.na(c) | valid==2, 2, 1, missing = 1))
  }

}
