################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tsmokelessstatus
# AgeRange Sex Valid T12 T13 T15 PSU Stratum WStep1 agerange2 UR Region

tsmokelessstatus <- function(.data) {
  
  # variable names that are used in the function
  tsmokelessstatus_names <- c("sex", "t12", "t13", "t15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokelessstatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokelessstatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(t12cln = if_else(t12==1 | t12==2, 1, 2, missing = 2)) %>% 
      mutate(t13cln = if_else(t12==1 & (t13==1 | t13==2), 1, 2, missing = 2)) %>% 
      mutate(t13cln = replace(t13cln, t12==2 & (is.na(t13) | t13==2), 1)) %>% 
      mutate(t15cln = if_else(t12==2 & (t15==1 | t15==2), 1, 2, missing = 2)) %>% 
      mutate(t15cln = replace(t15cln, t12==1 & (is.na(t15) | t15==1), 1)) %>% 
      mutate(cln = if_else(t12cln==1 & t13cln==1 & t15cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(t12==1 & t13==2, "2) current user (non-daily)", NA)) %>% 
      mutate(c = replace(c, t12==1 & t13==1, "1) daily user")) %>% 
      mutate(c = replace(c, t12==2 & t15==1, "3) past user")) %>% 
      mutate(c = replace(c, t15==2, "4) never used")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else(t12==1 & (t13==1 | t13==2), "1) daily and non-daily users", "2) non-user", 
                         missing = "2) non-user")) %>% 
      mutate(d = factor(d))
  }
  
}
