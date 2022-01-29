################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tsmokestatus
# AgeRange Sex Valid T1 T2 T8 PSU Stratum WStep1 agerange2 UR Region

tsmokestatus <- function(.data) {
  
  # variable names that are used in the function
  tsmokestatus_names <- c("sex", "t1", "t2", "t8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokestatus_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokestatus_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2)) %>% 
      mutate(t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 2, missing = 2)) %>% 
      mutate(t2cln = replace(t2cln, t1==2 & (is.na(t2) | t2==2), 1)) %>% 
      mutate(t8cln = if_else(t1==2 & (t8==1 | t8==2), 1, 2, missing = 2)) %>% 
      mutate(t8cln = replace(t8cln, t1==1 & (is.na(t8) | t8==1), 1)) %>% 
      mutate(cln = if_else(t1cln==1 & t2cln==1 & t8cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      # Smoking status
      mutate(c = ifelse(t1==1 & t2==2, "2) current smoker (non-daily)", NA)) %>% 
      mutate(c = replace(c, t1==1 & t2==1, "1) daily smoker")) %>% 
      mutate(c = replace(c, t1==2 & t8==1, "3) former smoker")) %>% 
      mutate(c = replace(c, t8==2, "4) never smoked")) %>% 
      mutate(c = factor(c)) %>%
      # Percentage of current smokers
      mutate(d = if_else(t1==1 & (t2==1 | t2==2), "1) daily and non-daily smokers", 
                         "2) non-smoker", missing = "2) non-smoker")) %>%
      mutate(d = factor(d))
  }
  
}
