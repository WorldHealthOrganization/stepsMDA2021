################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ocleaningtool
# AgeRange Sex Valid O13a O13d Stratum PSU WStep1 agerange2 UR Region

ocleaningtool <- function(.data) {

  # variable names that are used in the function
  ocleaningtool_names <- c("sex", "o13a", "o13b", "o13c", "o13d", "o13e", "o13f", "o13g")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ocleaningtool_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ocleaningtool_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(o13acln = if_else((o13a==1 | o13a==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o13bcln = if_else((o13b==1 | o13b==2) & valid==1, 1, 2, missing = 2)) %>%
      mutate(o13ccln = if_else((o13c==1 | o13c==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o13dcln = if_else((o13d==1 | o13d==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o13ecln = if_else((o13e==1 | o13e==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o13fcln = if_else((o13f==1 | o13f==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o13gcln = if_else((o13g==1 | o13g==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(a = ifelse(o13a==1, "1) uses toothbrush", NA)) %>% 
      mutate(a = replace(a, o13a==2, "2) does not use toothbrush")) %>% 
      mutate(a = factor(a)) %>% 
      mutate(b = ifelse(o13b==1, "1) uses wooden tooth-picks", NA)) %>% 
      mutate(b = replace(b, o13b==2, "2) does not use wooden tooth-picks")) %>% 
      mutate(b = factor(b)) %>% 
      mutate(c = ifelse(o13c==1, "1) uses plastic tooth-picks", NA)) %>% 
      mutate(c = replace(c, o13c==2, "2) does not use plastic tooth-picks")) %>% 
      mutate(c = factor(c)) %>%
      mutate(d = ifelse(o13d==1, "1) uses floss", NA)) %>% 
      mutate(d = replace(d, o13d==2, "2) does not use floss")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(o13e==1, "1) uses charcoal", NA)) %>% 
      mutate(e = replace(e, o13e==2, "2) does not use charcoal")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(o13f==1, "1) uses chewstick", NA)) %>% 
      mutate(f = replace(f, o13f==2, "2) does not use chewstick")) %>% 
      mutate(f = factor(f)) %>%
      mutate(g = ifelse(o13g==1, "1) uses other", NA)) %>% 
      mutate(g = replace(g, o13g==2, "2) does not use other")) %>% 
      mutate(g = factor(g))
  }

}
