################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Pnotmeetingrecs
# AgeRange Sex Valid P1 P2 P3a P3b P4 P5 P6a P6b P7 P8 P9a P9b P10 P11 P12a P12b 
# P13 P14 P15a P15b PSU Stratum WStep1 agerange2 UR Region

pnotmeetingrecs <- function(.data) {

  # variable names that are used in the function
  pnotmeetingrecs_names <- c("sex", "p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", 
                             "p7", "p8", "p9a", "p9b", "p10", "p11", "p12a", "p12b", 
                             "p13", "p14", "p15a", "p15b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, pnotmeetingrecs_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(pnotmeetingrecs_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(p1t3 = ifelse(p1t3cln==1, p2*p3*8, NA)) %>% 
      mutate(p4t6 = ifelse(p4t6cln==1, p5*p6*4, NA)) %>% 
      mutate(p7t9 = ifelse(p7t9cln==1, p8*p9*4, NA)) %>% 
      mutate(p10t12 = ifelse(p10t12cln==1, p11*p12*8, NA)) %>% 
      mutate(p13t15 = ifelse(p13t15cln==1, p14*p15*4, NA)) %>% 
      mutate(ptotal = p1t3+p4t6+p7t9+p10t12+p13t15) %>% 
      mutate(c = if_else(ptotal>=600, "2) meets recs", "1) doesn't meet recs", missing = "1) doesn't meet recs")) %>% 
      mutate(c = factor(c)) %>% 
      # UPDATED: Calculate additional ≥1200 MET-minutes/week
      mutate(mets1200 = if_else(ptotal>=1200, "2) meets recs", "1) doesn't meet recs", missing = "1) doesn't meet recs")) %>% 
      mutate(mets1200 = factor(mets1200)) %>% 
      #
      mutate(cln = if_else(p1t3cln==1 & p4t6cln==1 & p7t9cln==1 & p10t12cln==1 & 
                             p13t15cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(p1) & is.na(p4) & is.na(p7) & is.na(p10) & is.na(p13), 2))
  }

}
