################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

cleanrecodep16 <- function(.data) {
  
  # variable names that are used in the function
  cleanrecodep16_names <- c("p16a", "p16b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cleanrecodep16_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cleanrecodep16_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(p16a = replace(p16a, p16a==15 & (is.na(p16b) | p16b==0 | p16b==15 | p16b==77 | p16b==88 | p16b==99), 0)) %>% 
      mutate(p16b = replace(p16b, p16a==15 & (is.na(p16b) | p16b==0 | p16b==15 | p16b==77 | p16b==88 | p16b==99), 15)) %>%
      mutate(p16a = replace(p16a, p16a==30 & (is.na(p16b) | p16b==0 | p16b==30 | p16b==77 | p16b==88 | p16b==99), 0)) %>% 
      mutate(p16b = replace(p16b, p16a==30 & (is.na(p16b) | p16b==0 | p16b==30 | p16b==77 | p16b==88 | p16b==99), 30)) %>% 
      mutate(p16a = replace(p16a, p16a==45 & (is.na(p16b) | p16b==0 | p16b==45 | p16b==77 | p16b==88 | p16b==99), 0)) %>% 
      mutate(p16b = replace(p16b, p16a==45 & (is.na(p16b) | p16b==0 | p16b==45 | p16b==77 | p16b==88 | p16b==99), 45)) %>% 
      mutate(p16a = replace(p16a, p16a==60 & (is.na(p16b) | p16b==0 | p16b==60 | p16b==77 | p16b==88 | p16b==99), 1)) %>% 
      mutate(p16b = replace(p16b, p16a==60 & (is.na(p16b) | p16b==0 | p16b==60 | p16b==77 | p16b==88 | p16b==99), 0)) %>% 
      mutate(p16a = replace(p16a, (p16a==7 & p16b==77) | (p16a==8 & p16b==88) | (p16a==9 & p16b==99), 0)) %>% 
      mutate(p16b = replace(p16b, (p16a==7 & p16b==77) | (p16a==8 & p16b==88) | (p16a==9 & p16b==99), 0)) %>% 
      mutate(p16b = replace(p16b, p16b==77 | p16b==88 | p16b==99, 0)) %>% 
      mutate(p16a = replace(p16a, p16a==77 | p16a==88 | p16a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p16amin = ifelse(p16a>=0 & p16a<=24, p16a*60, NA)) %>% 
      mutate(p16bmin = ifelse(p16b>=0 & p16b<=60, p16b, NA)) %>% 
      mutate(p16 = p16amin + p16bmin) %>% 
      # Cleans P16
      mutate(p16cln = if_else(p16>=0 & p16<1441 & valid==1, 1, 2, missing = 2))
  }
  
}