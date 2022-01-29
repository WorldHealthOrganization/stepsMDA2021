################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in TPdanger
# AgeRange Sex Valid TP1a TP1b TP1c PSU Stratum WStep1

tpdanger <- function(.data) {

  # variable names that are used in the function
  tpdanger_names <- c("sex", "tp1a", "tp1b", "tp1c")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpdanger_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpdanger_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(tp1a==1, "1) noticed information in newspapers", NA)) %>% 
      mutate(c = replace(c, tp1a==2, "2) did not notice information in newspapers")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(tp1b==1, "1) noticed information on TV", NA)) %>% 
      mutate(d = replace(d, tp1b==2, "2) did not notice information on TV")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(tp1c==1, "1) noticed information on radio", NA)) %>% 
      mutate(e = replace(e, tp1c==2, "2) did not notice information on radio")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(tp1b==1 | tp1c==1, "1) noticed information on TV or radio", NA)) %>% 
      mutate(f = replace(f, tp1b==2 & tp1c==2, "2) did not notice information on TV or radio")) %>% 
      mutate(f = factor(f)) %>% 
      mutate(tp1acln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp1bcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp1ccln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp1acln = replace(tp1acln, is.na(c), 2)) %>% 
      mutate(tp1bcln = replace(tp1bcln, is.na(d), 2)) %>% 
      mutate(tp1ccln = replace(tp1ccln, is.na(e), 2))
  }

}
