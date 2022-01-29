################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in TPcigpromos
# AgeRange Sex Valid TP3a TP3b TP3c TP3d TP3e TP3f PSU Stratum WStep1

tpcigpromos <- function(.data) {

  # variable names that are used in the function
  tpcigpromos_names <- c("sex", "tp3a", "tp3b", "tp3c", "tp3d", "tp3e", "tp3f")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, tpcigpromos_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(tpcigpromos_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(a = ifelse(tp3a==1, "1) noticed free samples", NA)) %>% 
      mutate(a = replace(a, tp3a==2, "2) did not notice free samples")) %>% 
      mutate(a = factor(a)) %>% 
      mutate(b = ifelse(tp3b==1, "1) noticed cigs on sale", NA)) %>% 
      mutate(b = replace(b, tp3b==2, "2) did not notice cigs on sale")) %>% 
      mutate(b = factor(b)) %>% 
      mutate(c = ifelse(tp3c==1, "1) noticed cig coupons", NA)) %>% 
      mutate(c = replace(c, tp3c==2, "2) did not notice cig coupons")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(tp3d==1, "1) noticed free gifts", NA)) %>% 
      mutate(d = replace(d, tp3d==2, "2) did not notice free gifts")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(tp3e==1, "1) noticed branded clothing", NA)) %>% 
      mutate(e = replace(e, tp3e==2, "2) did not notice branded clothing")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(tp3f==1, "1) noticed mail promos", NA)) %>% 
      mutate(f = replace(f, tp3f==2, "2) did not notice mail promos")) %>% 
      mutate(f = factor(f)) %>% 
      mutate(tp3acln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3acln = replace(tp3acln, is.na(a), 2)) %>% 
      mutate(tp3bcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3bcln = replace(tp3bcln, is.na(b), 2)) %>% 
      mutate(tp3ccln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3ccln = replace(tp3ccln, is.na(c), 2)) %>% 
      mutate(tp3dcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3dcln = replace(tp3dcln, is.na(d), 2)) %>% 
      mutate(tp3ecln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3ecln = replace(tp3ecln, is.na(e), 2)) %>% 
      mutate(tp3fcln = if_else(valid==1, 1, 2, missing = 2)) %>% 
      mutate(tp3fcln = replace(tp3fcln, is.na(f), 2)) %>% 
      mutate(allcln = if_else(tp3acln==1 & tp3bcln==1 & tp3ccln==1 & tp3dcln==1 & 
                                tp3ecln==1 & tp3fcln==1, 1, 2, missing = 2)) %>% 
      mutate(g = if_else(tp3a==1 | tp3b==1 | tp3c==1 | tp3d==1 | tp3e==1 | tp3f==1, "1) noticed any promotion", 
                         "2) did not notice any promotion", missing = "2) did not notice any promotion")) %>% 
      mutate(g = factor(g))
  }

}
