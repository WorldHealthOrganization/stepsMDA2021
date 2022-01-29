################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in ETStatus
# AgeRange Sex Valid ET1 ET2 PSU Stratum WStep1 agerange2 UR Region

etstatus <- function(.data) {
  
  # variable names that are used in the function
  etstatus_names <- c("sex", "et1", "et2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, etstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(etstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(et1cln = if_else(et1==1 | et1==2, 1, 2, missing = 2)) %>% 
      mutate(et2cln = if_else(et1==1 & (et2==1 | et2==2), 1, 2, missing = 2)) %>% 
      mutate(et2cln = replace(et2cln, et1==2 & (is.na(et2) | et2==2), 1)) %>% 
      mutate(cln = if_else(et1cln==1 & et2cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(et1==1 & et2==2, "2) current user (non-daily)", NA)) %>% 
      mutate(c = replace(c, et1==1 & et2==1, "1) current daily user")) %>% 
      mutate(c = replace(c, et1==2, "3) former user or never used")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else(et1==1 & (et2==1 | et2==2), "1) daily and non-daily users", "2) non-users", 
                         missing = "2) non-users")) %>% 
      mutate(d = factor(d))
  }
  
}
