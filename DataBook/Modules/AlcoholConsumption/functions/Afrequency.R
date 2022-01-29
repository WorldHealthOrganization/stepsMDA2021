################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Afrequency
# AgeRange Sex Valid A1 A2 A4 PSU Stratum WStep1 agerange2 UR Region

afrequency <- function(.data) {

  # variable names that are used in the function
  afrequency_names <- c("sex", "a1", "a2", "a4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, afrequency_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(afrequency_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(c = ifelse(a4==1, "1) Daily",
                        ifelse(a4==2, "2) 5-6 days per week",
                               ifelse(a4==3, "3) 3-4 days per week",
                                      ifelse(a4==4, "4) 1-2 days per week",
                                             ifelse(a4==5, "5) 1-3 days per month",
                                                    ifelse(a4==6, "6) less than once a month",
                                                           ifelse(a4==7, "7) Never", NA)))))))) %>% 
      mutate(c = factor(c)) %>% 
      mutate(cln = if_else(a1==1 & a2==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(c), 2))
  }

}
