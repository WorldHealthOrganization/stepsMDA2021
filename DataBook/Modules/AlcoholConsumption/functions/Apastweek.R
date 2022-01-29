################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Apastweek
# AgeRange Sex Valid A1 A2 A5 A10a A10b A10c A10d A10e A10f A10g PSU Stratum WStep1 agerange2 UR Region

apastweek <- function(.data) {
  
  apastweek_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", "a10d", "a10e", "a10f", "a10g")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, apastweek_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(apastweek_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(allmissing = if_else((is.na(a10a) | a10a>=77) & (is.na(a10b) | a10b>=77) 
                                  & (is.na(a10c) | a10c>=77), 1, 2, missing = 2)) %>% 
      mutate(allmissing = if_else((is.na(a10d) | a10d>=77) & (is.na(a10e) | a10e>=77) 
                                  & (is.na(a10f) | a10f>=77) & (is.na(a10g) | a10g>=77), 1, 2, missing = 2)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & (is.na(a10a) | a10a>=77), 0)) %>% 
      mutate(a10b = replace(a10b, allmissing==2 & (is.na(a10b) | a10b>=77), 0)) %>% 
      mutate(a10c = replace(a10c, allmissing==2 & (is.na(a10c) | a10c>=77), 0)) %>% 
      mutate(a10d = replace(a10d, allmissing==2 & (is.na(a10d) | a10d>=77), 0)) %>% 
      mutate(a10e = replace(a10e, allmissing==2 & (is.na(a10e) | a10e>=77), 0)) %>% 
      mutate(a10f = replace(a10f, allmissing==2 & (is.na(a10f) | a10f>=77), 0)) %>% 
      mutate(a10g = replace(a10g, allmissing==2 & (is.na(a10g) | a10g>=77), 0)) %>% 
      mutate(a10cln = if_else(a10a<51 & a10b<51 & a10c<51 & a10d<51 & a10e<51 & a10f<51 
                              & a10g<51, 1, 2, missing = 2)) %>% 
      mutate(cln = if_else(a1==1 & a2==1 & a5==1 & a10cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(a10acount = if_else(a10a>0 & a10a<51, 1, 0, missing = 0)) %>% 
      mutate(a10bcount = if_else(a10b>0 & a10b<51, 1, 0, missing = 0)) %>% 
      mutate(a10ccount = if_else(a10c>0 & a10c<51, 1, 0, missing = 0)) %>% 
      mutate(a10dcount = if_else(a10d>0 & a10d<51, 1, 0, missing = 0)) %>% 
      mutate(a10ecount = if_else(a10e>0 & a10e<51, 1, 0, missing = 0)) %>% 
      mutate(a10fcount = if_else(a10f>0 & a10f<51, 1, 0, missing = 0)) %>% 
      mutate(a10gcount = if_else(a10g>0 & a10g<51, 1, 0, missing = 0)) %>% 
      mutate(days = a10acount+a10bcount+a10ccount+a10dcount+a10ecount+a10fcount+a10gcount) %>% 
      mutate(c = ifelse(days==7, "1) daily", NA)) %>% 
      mutate(c = replace(c, days==6 | days==5, "2) 5-6 days")) %>% 
      mutate(c = replace(c, days==4 | days==3, "3) 3-4 days")) %>% 
      mutate(c = replace(c, days==2 | days==1, "4) 1-2 days")) %>% 
      mutate(c = replace(c, days==0, "5) 0 days")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(meandrinksperday = (a10a+a10b+a10c+a10d+a10e+a10f+a10g)/7)
  }
  
}

