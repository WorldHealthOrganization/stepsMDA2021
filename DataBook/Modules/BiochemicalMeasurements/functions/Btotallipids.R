################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Btotallipids
# AgeRange Sex Valid B8 B9 PSU Stratum WStep3 agerange2 UR Region

btotallipids <- function(.data) {

  # variable names that are used in the function
  btotallipids_names <- c("sex", "b8", "b9")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, btotallipids_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(btotallipids_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else(b8>=2 & b8<=12 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(b8<5.0, "total cholesterol < 5.0", NA)) %>% 
      mutate(d = ifelse(b8<5.0, "total cholesterol < 6.2", NA)) %>% 
      mutate(e = ifelse(b8<5.0, "total cholesterol < 5.0", NA)) %>% 
      mutate(f = ifelse(b8<5.0, "total cholesterol < 6.2", NA)) %>% 
      mutate(c = replace(c, b8>=5.0 & b8<6.2, "total cholesterol >= 5.0 or on meds")) %>% 
      mutate(d = replace(d, b8>=5.0 & b8<6.2, "total cholesterol < 6.2")) %>% 
      mutate(e = replace(e, b8>=5.0 & b8<6.2, "total cholesterol >= 5.0")) %>% 
      mutate(f = replace(f, b8>=5.0 & b8<6.2, "total cholesterol < 6.2")) %>% 
      mutate(c = replace(c, b8>=6.2, "total cholesterol >= 5.0 or on meds")) %>% 
      mutate(d = replace(d, b8>=6.2, "total cholesterol >= 6.2 or on meds")) %>% 
      mutate(e = replace(e, b8>=6.2, "total cholesterol >= 5.0")) %>% 
      mutate(f = replace(f, b8>=6.2, "total cholesterol >= 6.2")) %>% 
      mutate(c = replace(c, b9==1, "total cholesterol >= 5.0 or on meds")) %>% 
      mutate(d = replace(d, b9==1, "total cholesterol >= 6.2 or on meds")) %>% 
      mutate(b8mg = b8*38.67) %>% 
      # need to mutate into factors for cases when there are zero values in some age ranges
      mutate(c = factor(c)) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = factor(f))
  }

}
