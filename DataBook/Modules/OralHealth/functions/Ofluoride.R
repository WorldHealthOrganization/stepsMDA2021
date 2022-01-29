################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Ofluoride
# AgeRange Sex Valid O11 O12 Stratum PSU WStep1

ofluoride <- function(.data) {

  # variable names that are used in the function
  ofluoride_names <- c("sex", "o11", "o12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, ofluoride_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(ofluoride_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((o11==1 | o11==2) & (o12==1 | o12==2) & valid==1, 1, 2, missing = 2)) %>% 
      # those who don't use toothpaste and is NA in fluoride question
      mutate(cln = replace(cln, o11==2 & is.na(o12), 2)) %>% 
      # those who don't use toothpaste but replied "yes" to fluoride question
      mutate(cln = replace(cln, o1==2 & o12==1, 2)) %>% 
      mutate(c = ifelse(o12==1, "1) uses toothpaste w/ fluoride", NA)) %>% 
      mutate(c = replace(c, o12==2 | o11==2, "2) does not use toothpaste w/ fluoride")) %>% 
      mutate(c = factor(c))
  }

}
