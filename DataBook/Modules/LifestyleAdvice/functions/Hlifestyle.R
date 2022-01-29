################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hlifestyle
# AgeRange Sex Valid H20a H20b H20c H20d H20e H20f H20g PSU Stratum WStep1 agerange2 UR Region

hlifestyle <- function(.data) {
  
  hlifestyle_names <- c("sex", "h20a", "h20b", "h20c", "h20d", "h20e", "h20f", "h20g")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hlifestyle_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hlifestyle_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(h20acln = if_else((h20a==1 | h20a==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20bcln = if_else((h20b==1 | h20b==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20ccln = if_else((h20c==1 | h20c==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20dcln = if_else((h20d==1 | h20d==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20ecln = if_else((h20e==1 | h20e==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20fcln = if_else((h20f==1 | h20f==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h20gcln = if_else((h20g==1 | h20g==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(a = ifelse(h20a==1, "advised to quit/not start tob", NA)) %>% 
      mutate(a = replace(a, h20a==2, "not advised to quit/not start tob")) %>% 
      mutate(a = factor(a)) %>% 
      mutate(b = ifelse(h20b==1, "advised to reduce salt", NA)) %>% 
      mutate(b = replace(b, h20b==2, "not advised to reduce salt")) %>% 
      mutate(b = factor(b)) %>% 
      mutate(c = ifelse(h20c==1, "advised to eat more fruit/veg", NA)) %>% 
      mutate(c = replace(c, h20c==2, "not advised to eat more fruit/veg")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h20d==1, "advised to reduce fat in diet", NA)) %>% 
      mutate(d = replace(d, h20d==2, "not advised to reduce fat in diet")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(h20e==1, "advised to do more physical activity", NA)) %>% 
      mutate(e = replace(e, h20e==2, "not advised to do more physical activity")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(h20f==1, "advised to maintain/lose weight", NA)) %>% 
      mutate(f = replace(f, h20f==2, "not advised to maintain/lose weight")) %>% 
      mutate(f = factor(f)) %>% 
      mutate(g = ifelse(h20g==1, "advised to reduce sugary beverages in diet", NA)) %>% 
      mutate(g = replace(g, h20g==2, "not advised to reduce sugary beverages in diet")) %>% 
      mutate(g = factor(g))
  }
  
}

