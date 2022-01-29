################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Hmammogram
# Age AgeRange Sex Valid CA1 CA3b Stratum PSU WStep1 agerange2 UR Region

hmammogram <- function(.data) {

  # variable names that are used in the function
  hmammogram_names <- c("sex", "ca1", "ca3b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, hmammogram_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(hmammogram_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((ca3b==1 | ca3b==2 | ca3b==3 | ca3b==4 | ca3b==5) & 
                             ca1==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(ca3b==1, "1) In the last 12 months", NA)) %>% 
      mutate(c = ifelse(ca3b==2, "2) More than 1, less than 2 years ago", c)) %>% 
      mutate(c = ifelse(ca3b==3, "3) More than 2, less than 5 years ago", c)) %>% 
      mutate(c = ifelse(ca3b==4, "4) More than 5 years ago", c)) %>% 
      mutate(c = ifelse(ca3b==5, "5) Never", c)) %>% 
      mutate(c = factor(c))
  }

}
