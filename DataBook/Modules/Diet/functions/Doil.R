################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Doil
# AgeRange Sex Valid D12 PSU Stratum WStep1 agerange2 UR Region

doil <- function(.data) {

  # variable names that are used in the function
  doil_names <- c("sex", "d12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, doil_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(doil_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(d12cln = if_else(is.na(d12) | d12>7, 2, 1, missing = 1)) %>% 
      mutate(cln = if_else(d12cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      # use ifelse and not replace because otherwise you would need to replace 77 with NA too
      mutate(c = ifelse(d12==1, "1) Vegetable oil",
                        ifelse(d12==2, "2) Lard or Suet",
                               ifelse(d12==3, "3) Butter or Ghee", 
                                      ifelse(d12==4, "4) Margarine", 
                                             ifelse(d12==5, "5) Other",
                                                    ifelse(d12==6, "6) None in particular",
                                                           ifelse(d12==7, "7) None used", NA)))))))) %>%
      mutate(c = factor(c)) 
  }

}
