################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Pnoactivitybyset
# AgeRange Sex Valid P1 P2 P3a P3b P4 P5 P6a P6b P7 P8 P9a P9b P10 P11 P12a P12b 
# P13 P14 P15a P15b PSU Stratum WStep1 agerange2 UR Region

pnoactivitybyset <- function(.data) {

  # variable names that are used in the function
  pnoactivitybyset_names <- c("sex", "p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", 
                              "p7", "p8", "p9a", "p9b", "p10", "p11", "p12a", "p12b", 
                              "p13", "p14", "p15a", "p15b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, pnoactivitybyset_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(pnoactivitybyset_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(work = if_else(p1==1 | p4==1, "did work activity", "did no work activity", 
                            missing = "did no work activity")) %>% 
      mutate(work = factor(work)) %>% 
      mutate(trans = if_else(p7==1, "did transport activity", "did no transport activity", 
                             missing = "did no transport activity")) %>% 
      mutate(trans = factor(trans)) %>% 
      mutate(rec = if_else(p10==1 | p13==1, "did recreation activity", "did no recreation activity", 
                           missing = "did no recreation activity")) %>% 
      mutate(rec = factor(rec)) %>% 
      mutate(cln = if_else(p1t3cln==1 & p4t6cln==1 & p7t9cln==1 & p10t12cln==1 & 
                             p13t15cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = replace(cln, is.na(p1) & is.na(p4) & is.na(p7) & is.na(p10) & is.na(p13), 2))
  }

}
