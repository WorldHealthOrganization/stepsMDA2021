################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hcvdmeds
# AgeRange Sex Valid H18 H19 Stratum PSU WStep1 agerange2 UR Region
# NOTE: "h17" added to include those that have CVD

hcvdmeds <- function(.data) {

  # variable names that are used in the function
  hcvdmeds_names <- c("sex", "h17", "h18", "h19")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hcvdmeds_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hcvdmeds_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(h18cln = if_else((h18==1 | h18==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h19cln = if_else((h19==1 | h19==2) & valid==1, 1, 2, missing = 2)) %>% 
      # subset of people with CVD (added to the original script)
      mutate(h17cln = if_else(h17==1, 1, 2, missing = 2)) %>% 
      mutate(h18cln1 = if_else(h17cln==1 & (h18==1 | h18==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(h19cln1 = if_else(h17cln==1 & (h19==1 | h19==2) & valid==1, 1, 2, missing = 2)) %>% 
      ###
      mutate(c = ifelse(h18==1, "1) taking aspirin", NA)) %>% 
      mutate(c = replace(c, h18==2, "2) not taking aspirin")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(h19==1, "1) taking statins", NA)) %>% 
      mutate(d = replace(d, h19==2, "2) not taking statins")) %>% 
      mutate(d = factor(d))
  }

}
