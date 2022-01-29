################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Oproblem
# AgeRange Sex Valid O14a O14b O14c O14d O14e O14f O14g O14h O14i O14j 
# Stratum PSU WStep1 agerange2 UR Region

library(tibble)

oproblem <- function(.data) {
  
  # variable names that are used in the function
  oproblem_names <- c("sex", "o14a", "o14b", "o14c", "o14d", "o14e", "o14f", 
                      "o14g", "o14h", "o14i", "o14j")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, oproblem_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(oproblem_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(o14acln = if_else((o14a==1 | o14a==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14bcln = if_else((o14b==1 | o14b==2) & valid==1, 1, 2, missing = 2)) %>%
      mutate(o14ccln = if_else((o14c==1 | o14c==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14dcln = if_else((o14d==1 | o14d==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14ecln = if_else((o14e==1 | o14e==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14fcln = if_else((o14f==1 | o14f==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14gcln = if_else((o14g==1 | o14g==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14hcln = if_else((o14h==1 | o14h==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14icln = if_else((o14i==1 | o14i==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o14jcln = if_else((o14j==1 | o14j==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(a = ifelse(o14a==1, "1) had chewing difficulty", NA)) %>% 
      mutate(a = replace(a, o14a==2, "2) didn't have chewing difficulty")) %>% 
      mutate(a = factor(a)) %>% 
      mutate(b = ifelse(o14b==1, "1) had speech difficulty", NA)) %>% 
      mutate(b = replace(b, o14b==2, "2) didn't have speech difficulty")) %>% 
      mutate(b = factor(b)) %>% 
      mutate(c = ifelse(o14c==1, "1) felt tense", NA)) %>% 
      mutate(c = replace(c, o14c==2, "2) didn't feel tense")) %>% 
      mutate(c = factor(c)) %>%
      mutate(d = ifelse(o14d==1, "1) felt embarrassed", NA)) %>% 
      mutate(d = replace(d, o14d==2, "2) didn't feel embarrassed")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(o14e==1, "1) avoided smiling", NA)) %>% 
      mutate(e = replace(e, o14e==2, "2) didn't avoid smiling")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(o14f==1, "1) had interrupted sleep", NA)) %>% 
      mutate(f = replace(f, o14f==2, "2) didn't have interrupted sleep")) %>% 
      mutate(f = factor(f)) %>%
      mutate(g = ifelse(o14g==1, "1) missed work", NA)) %>% 
      mutate(g = replace(g, o14g==2, "2) didn't miss work")) %>% 
      mutate(g = factor(g)) %>% 
      mutate(h = ifelse(o14h==1, "1) had difficulty doing activities", NA)) %>% 
      mutate(h = replace(h, o14h==2, "2) didn't have difficulty doing activities")) %>% 
      mutate(h = factor(h)) %>% 
      mutate(i = ifelse(o14i==1, "1) was less tolerant", NA)) %>% 
      mutate(i = replace(i, o14i==2, "2) wasn't less tolerant")) %>% 
      mutate(i = factor(i)) %>%
      mutate(j = ifelse(o14j==1, "1) reduced social activities", NA)) %>% 
      mutate(j = replace(j, o14j==2, "2) didn't reduce social activities")) %>% 
      mutate(j = factor(j))
  }
  
}
