################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Mbmi
# AgeRange Sex Valid M11 M12 M8 Stratum PSU WStep2 agerange2 UR Region

mbmi <- function(.data) {

  # variable names that are used in the function
  mbmi_names <- c("sex", "m8", "m11", "m12")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mbmi_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mbmi_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(bmi = (m12/(m11*m11))*10000) %>% 
      mutate(m11cln = if_else(m11>=100 & m11<=270 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(m12cln = if_else(m12>=20 & m12<=350 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(bmicln = if_else(m12cln==1 & m11cln==1, 1, 2, missing = 2)) %>% 
      mutate(bmicln = replace(bmicln, (sex=="Women" & m8==1) | (bmi<14 | bmi>60), 2)) %>% 
      mutate(m11cln = replace(m11cln, sex=="Women" & m8==1, 2)) %>%
      mutate(m12cln = replace(m12cln, sex=="Women" & m8==1, 2))
  }

}
