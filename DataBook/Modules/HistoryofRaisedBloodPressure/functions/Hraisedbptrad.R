################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hraisedbptrad
# AgeRange Sex Valid H1 H2a H4 H5 PSU Stratum WStep1 agerange2 UR Region

hraisedbptrad <- function(.data) {
  
  hraisedbptrad_names <- c("sex", "h1", "h2a", "h4", "h5")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hraisedbptrad_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hraisedbptrad_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(cln = if_else(h1==1 & h2a==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h4cln = if_else(cln==1 & h4==1 | h4==2, 1, 2, missing = 2)) %>% 
      mutate(h5cln = if_else(cln==1 & h5==1 | h5==2, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(h4==1, "1) has seen a traditional healer", NA)) %>% 
      mutate(c = replace(c, h4==2, "2) has not seen a traditional healer")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h5==1, "1) taking herbal/traditional remedy", NA)) %>% 
      mutate(d = replace(d, h5==2, "2) not taking herbal/traditional remedy")) %>% 
      mutate(d = factor(d))
  }
  
}

