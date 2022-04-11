################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Cethnic
# AgeRange Sex Valid C6 UR Region agerange2

cethnic <- function(.data) {

  # variable names that are used in the function
  cethnic_names <- c("sex", "c6")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cethnic_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cethnic_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(is.na(c6) | c6==88 | c6==77 | valid==2, 2, 1, missing = 1)) %>% 
      # added for MDA to label output columns
      mutate(c = ifelse(c6==1, "1) Romanian/Moldovan", NA)) %>% 
      mutate(c = replace(c, c6==2, "2) Ukrainian")) %>% 
      mutate(c = replace(c, c6==3, "3) Russian")) %>% 
      mutate(c = replace(c, c6==4, "4) Gagauz")) %>% 
      mutate(c = replace(c, c6==5, "5) Roma")) %>% 
      mutate(c = replace(c, c6==6, "6) Other ethnic group")) %>% 
      mutate(c = factor(c))
  }

}
