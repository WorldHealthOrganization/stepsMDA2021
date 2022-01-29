################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tdailyuser
# AgeRange Sex Valid T1 T2 T8 T12 T13 T15 PSU Stratum WStep1 agerange2 UR Region

tdailyuser <- function(.data) {
  
  # variable names that are used in the function
  tdailyuser_names <- c("sex", "t1", "t2", "t8", "t12", "t13", "t15")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tdailyuser_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tdailyuser_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2)) %>% 
      mutate(t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 2, missing = 2)) %>% 
      mutate(t2cln = replace(t2cln, t1==2 & (is.na(t2) | t2==2), 1)) %>% 
      mutate(t8cln = if_else(t1==2 & (t8==1 | t8==2), 1, 2, missing = 2)) %>% 
      mutate(t8cln = replace(t8cln, t1==1 & (is.na(t8) | t8==1), 1)) %>% 
      mutate(t12cln = if_else(t12==1 | t12==2, 1, 2, missing = 2)) %>% 
      mutate(t13cln = if_else(t12==1 & (t13==1 | t13==2), 1, 2, missing = 2)) %>% 
      mutate(t13cln = replace(t13cln, t12==2 & (is.na(t13) | t13==2), 1)) %>% 
      mutate(t15cln = if_else(t12==2 & (t15==1 | t15==2), 1, 2, missing = 2)) %>% 
      mutate(t15cln = replace(t15cln, t12==1 & (is.na(t15) | t15==1), 1)) %>% 
      mutate(cln = if_else(t1cln==1 & t2cln==1 & t8cln==1 & t12cln==1 & t13cln==1 & 
                             t15cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = if_else(t1==1 | t12==1, "1) current tobacco user", "2) not current tobacco user", 
                         missing = "2) not current tobacco user")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = if_else((t1==1 & t2==1) | (t12==1 & t13==1), "1) current daily user", 
                         "2) not current daily user", missing = "2) not current daily user")) %>% 
      mutate(d = factor(d)) %>% 
      # next var e isn't used in Data Book
      mutate(e = ifelse(t1==1 | t12==1, "1) current tobacco user", NA)) %>% 
      mutate(e = replace(e, t1==2 & t12==2 & (t8==1 | t15==1), "2) past tobacco user")) %>% 
      mutate(e = replace(e, t1==2 & t12==2 & t8==2 & t15==2, "3) never user")) %>% 
      mutate(e = factor(e))
  }
  
}

