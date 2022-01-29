################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in HTSmokeagetime
# AgeRange Sex Valid Age HT1 HT2 HT3 HT4 HT4type Stratum PSU WStep1 agerange2 UR Region

htsmokeagetime <- function(.data) {
  
  # variable names that are used in the function
  htsmokeagetime_names <- c("sex", "htp1", "htp2", "htp3", "htp4", "htp4type")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, htsmokeagetime_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(htsmokeagetime_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(htp1cln = if_else(htp1==1 | htp1==2, 1, 2, missing = 2)) %>% 
      mutate(htp2cln = if_else(htp1==1 & (htp2==1 | htp2==2), 1, 2, missing = 2)) %>% 
      mutate(htp2cln = replace(htp2cln, htp1==2 & (is.na(htp2) | htp2==2), 1)) %>% 
      mutate(htp4a = ifelse(htp4type=="years", htp4, NA)) %>% 
      mutate(htp4byrs = ifelse(htp4type=="months", htp4/12, NA)) %>% 
      mutate(htp4cyrs = ifelse(htp4type=="weeks", htp4/52, NA)) %>% 
      mutate(htp3cln = if_else(htp3>=7 & htp3<70, 1, 2, missing = 2)) %>% 
      mutate(htp4acln = if_else(htp4a>0 & htp4a<=61, 1, 2, missing = 2)) %>% 
      mutate(htp4bcln = if_else(is.na(htp4byrs), 2, 1, missing = 1)) %>% 
      mutate(htp4ccln = if_else(is.na(htp4cyrs), 2, 1, missing = 1)) %>% 
      mutate(initiation = ifelse(htp4ccln==1, age-htp4cyrs, 
                                 ifelse(htp4bcln==1, age-htp4byrs,
                                        ifelse(htp4acln==1, age-htp4a, 
                                               ifelse(htp3cln==1, htp3, NA))))) %>% 
      mutate(duration = age - initiation) %>% 
      mutate(cln = if_else(htp1cln==1 & htp2cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, initiation<7 | is.na(initiation) | duration<0, 2))
  }
  
}