################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hcholtrad
# AgeRange Sex Valid H12 H13a H15 H16 PSU Stratum WStep1 agerange2 UR Region

hcholtrad <- function(.data) {
  
  hcholtrad_names <- c("sex", "h12", "h13a", "h13b", "h14")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcholtrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcholtrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(cln = if_else(h12==1 & h13a==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h15cln = if_else(cln==1 & h15==1 | h15==2, 1, 2, missing = 2)) %>% 
      mutate(h16cln = if_else(cln==1 & h16==1 | h16==2, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h15==1, "1) has seen a traditional healer", NA)) %>% 
      mutate(c = replace(c, h15==2, "2) has not seen a traditional healer")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h16==1, "1) taking herbal/traditional remedy", NA)) %>% 
      mutate(d = replace(d, h16==2, "2) not taking herbal/traditional remedy")) %>% 
      mutate(d = factor(d))
  }
  
}

