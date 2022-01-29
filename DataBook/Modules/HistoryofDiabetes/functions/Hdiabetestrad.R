################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hdiabetestrad
# AgeRange Sex Valid H6 H7a H10 H11 Stratum PSU WStep1 agerange2 UR Region

hdiabetestrad <- function(.data) {
  
  hdiabetestrad_names <- c("sex", "h6", "h7a", "h10", "h11")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hdiabetestrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hdiabetestrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(h10cln = if_else(h6==1 & h7a==1 & (h10==1 | h10==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h11cln = if_else(h6==1 & h7a==1 & (h11==1 | h11==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h10==1, "1) has seen a traditional healer", NA)) %>% 
      mutate(c = replace(c, h10==2, "2) has not seen a traditional healer")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h11==1, "1) taking herbal/traditional remedy", NA)) %>% 
      mutate(d = replace(d, h11==2, "2) not taking herbal/traditional remedy")) %>% 
      mutate(d = factor(d))
  }
  
}

