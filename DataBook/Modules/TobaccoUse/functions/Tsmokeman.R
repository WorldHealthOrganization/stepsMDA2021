################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Tsmokeman
# AgeRange Sex Valid T1 T2 T5a T5aw PSU Stratum WStep1 agerange2 UR Region

tsmokeman <- function(.data) {
  
  # variable names that are used in the function
  tsmokeman_names <- c("sex", "t1", "t2", "t5a", "t5aw")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tsmokeman_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tsmokeman_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(t1cln = if_else(t1==1 | t1==2, 1, 2, missing = 2)) %>% 
      mutate(t2cln = if_else(t1==1 & (t2==1 | t2==2), 1, 2, missing = 2)) %>% 
      mutate(t2cln = replace(t2cln, t1==2 & (is.na(t2) | t2==2), 1)) %>% 
      mutate(cln = if_else(t1cln==1 & t2cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, t2==1 & (is.na(t5a) | t5a>50) & (is.na(t5aw) | t5aw>350), 2)) %>% 
      mutate(cln = replace(cln, t2==2 & (is.na(t5aw) | t5aw>350), 2)) %>% 
      mutate(c = if_else((t5aw>=1 & t5aw<=350) | (t2==1 & t5a>=1 & t5a<=50), "smokes manufactured cig", 
                         "does not smoke manufactured cig", missing = "does not smoke manufactured cig")) %>% 
      mutate(c = factor(c))
  }
  
}