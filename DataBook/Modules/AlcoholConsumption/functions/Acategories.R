################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Acategories
# AgeRange Sex Valid A1 A2 A5 A7 PSU Stratum WStep1 agerange2 UR Region

acategories <- function(.data) {

  # variable names that are used in the function
  acategories_names <- c("sex", "a1", "a2", "a5", "a7")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, acategories_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(acategories_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(a1cln = if_else(a1==1 | a1==2, 1, 2, missing = 2)) %>%
      mutate(a2cln = if_else(a1==1 & (a2==1 | a2==2), 1, 2, missing = 2)) %>%
      mutate(a2cln = replace(a2cln, a1==2 & (is.na(a2) | a2==2), 1)) %>%
      mutate(a5cln = if_else(a2==1 & (a5==1 | a5==2), 1, 2, missing = 2)) %>%
      mutate(a5cln = replace(a5cln, a2==2 & (is.na(a5) | a5==2), 1)) %>%
      mutate(a5cln = replace(a5cln, a1==2 & (is.na(a5) | a5==2), 1)) %>%
      mutate(a7 = replace(a7, a7==77 | a7==88 | a7==99, NA)) %>%
      mutate(a7cln = if_else(a7>=1 & a7<=50, 1, 2, missing = 2)) %>%
      mutate(cln = if_else(a1cln==1 & a2cln==1 & a5cln==1 & valid==1, 1, 2, missing = 2)) %>%
      mutate(cln = replace(cln, a1==1 & a2==1 & a5==1 & a7cln==2, 2)) %>%
      mutate(cln = replace(cln, (a5==2 | is.na(a5)) & a7>1, 2)) %>%
      mutate(clndrinker = if_else(a1==1 & a2==1 & a5==1 & a7cln==1 & valid==1, 1, 2, missing = 2)) %>%
      ###
      mutate(c = ifelse(a7<4, "3) lower-end", NA)) %>%
      mutate(c = replace(c, a7>=4 & a7<6, "2) intermed level")) %>%
      mutate(c = replace(c, a7>=6, "1) high-end")) %>%
      mutate(c = replace(c, a5==2 | is.na(a5), "not a current drinker")) %>%
      mutate(c = factor(c)) %>%
      mutate(d = ifelse(a7<2, "3) lower-end", NA)) %>%
      mutate(d = replace(d, a7>=2 & a7<4, "2) intermed level")) %>%
      mutate(d = replace(d, a7>=4, "1) high-end")) %>%
      mutate(d = replace(d, a5==2 | is.na(a5), "not a current drinker")) %>%
      mutate(d = factor(d)) %>%
      mutate(e = if_else(sex=="Men", c, d, missing = d)) %>%
      mutate(e = factor(e))
  }

}

