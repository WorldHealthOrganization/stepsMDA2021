################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# MODDED FOR MDA

# Variables used in Oreasonvisit
# AgeRange Sex Valid O8 O9 Stratum PSU WStep1 agerange2 UR Region

oreasonvisit <- function(.data) {

  # variable names that are used in the function
  oreasonvisit_names <- c("sex", "o8", "o9", "ox2")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, oreasonvisit_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(oreasonvisit_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(o8>=1 & o8<=5 & o9>=1 & o9<=5 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o9==1, "1) consultation/advice", NA)) %>% 
      mutate(c = replace(c, o9==2, "2) pain")) %>% 
      mutate(c = replace(c, o9==3, "3) follow-up")) %>% 
      mutate(c = replace(c, o9==4, "4) check-up")) %>% 
      mutate(c = replace(c, o9==5, "5) other")) %>% 
      mutate(c = factor(c)) %>% 
      # extra for MDA
      mutate(clnox2 = if_else(o8>2 & o8<6 & ox2>=1 & ox2<=5 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(d = ifelse(ox2==1, "1) did not need it", NA)) %>% 
      mutate(d = replace(d, ox2==2, "2) did not have money")) %>% 
      mutate(d = replace(d, ox2==3, "3) there is not a dentist in the neighborhood")) %>% 
      mutate(d = replace(d, ox2==4, "4) fear")) %>% 
      mutate(d = replace(d, ox2==5, "5) other")) %>% 
      mutate(d = factor(d))
      
  }

}
