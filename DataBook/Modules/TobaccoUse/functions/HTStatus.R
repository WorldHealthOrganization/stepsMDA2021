################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in HTStatus
# AgeRange Sex Valid HTP1 HTP2 PSU Stratum WStep1 agerange2 UR Region

htstatus <- function(.data) {
  
  # variable names that are used in the function
  htstatus_names <- c("sex", "htp1", "htp2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, htstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(htstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(htp1cln = if_else(htp1==1 | htp1==2, 1, 2, missing = 2)) %>% 
      mutate(htp2cln = if_else(htp1==1 & (htp2==1 | htp2==2), 1, 2, missing = 2)) %>% 
      mutate(htp2cln = replace(htp2cln, htp1==2 & (is.na(htp2) | htp2==2), 1)) %>% 
      mutate(cln = if_else(htp1cln==1 & htp2cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(htp1==1 & htp2==2, "2) current user (non-daily)", NA)) %>% 
      mutate(c = replace(c, htp1==1 & htp2==1, "1) current daily user")) %>% 
      mutate(c = replace(c, htp1==2, "3) former user or never used")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else(htp1==1 & (htp2==1 | htp2==2), "1) daily and non-daily users", "2) non-users", 
                         missing = "2) non-users")) %>% 
      mutate(d = factor(d))
  }
  
}
