################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Mbloodpressure
# AgeRange Sex Valid M4a M4b M5a M5b M6a M6b PSU Stratum WStep2 agerange2 UR Region

mbloodpressure <- function(.data) {

  # variable names that are used in the function
  mbloodpressure_names <- c("sex", "m4a", "m4b", "m5a", "m5b", "m6a", "m6b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mbloodpressure_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mbloodpressure_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2)) %>% 
      mutate(m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2)) %>% 
      mutate(m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2)) %>% 
      mutate(m4bcln = if_else(m4b>=30 & m4b<=200, 1, 2, missing = 2)) %>% 
      mutate(m5bcln = if_else(m5b>=30 & m5b<=200, 1, 2, missing = 2)) %>% 
      mutate(m6bcln = if_else(m6b>=30 & m6b<=200, 1, 2, missing = 2)) %>% 
      mutate(sbp = ifelse((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1, (m5a+m6a)/2, 
                          ifelse(m4acln==1 & m5acln==2 & m6acln==1, (m4a+m6a)/2, 
                                 ifelse(m4acln==1 & m5acln==1 & m6acln==2, (m4a+m5a)/2, NA)))) %>% 
      mutate(dbp = ifelse((m4bcln==1 | m4bcln==2) & m5bcln==1 & m6bcln==1, (m5b+m6b)/2, 
                          ifelse(m4bcln==1 & m5bcln==2 & m6bcln==1, (m4b+m6b)/2, 
                                 ifelse(m4bcln==1 & m5bcln==1 & m6bcln==2, (m4b+m5b)/2, NA)))) %>% 
      mutate(sbpcln = if_else((sbp<40 | sbp>300) | is.na(sbp) | valid==2, 2, 1, missing = 2)) %>% 
      mutate(dbpcln = if_else((dbp<30 | dbp>200) | is.na(dbp) | valid==2, 2, 1, missing = 2))
  }

}
