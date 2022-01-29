################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Dfiveormore
# AgeRange Sex Valid D1 D2 D3 D4 PSU Stratum WStep1 agerange2 UR Region

dfiveormore <- function(.data) {

  # variable names that are used in the function
  dfiveormore_names <- c("sex", "d1", "d2", "d3", "d4")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, dfiveormore_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(dfiveormore_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(d2 = replace(d2, d1==0 & (is.na(d2) | d2>20), 0)) %>% 
      mutate(d4 = replace(d4, d3==0 & (is.na(d4) | d4>20), 0)) %>% 
      mutate(d2 = replace(d2, d1>=1 & d1<=7 & d2>20, NA)) %>% 
      mutate(d4 = replace(d4, d3>=1 & d3<=7 & d4>20, NA)) %>% 
      mutate(d1cln = if_else(is.na(d1) | d1>7, 2, 1, missing = 1)) %>% 
      mutate(d3cln = if_else(is.na(d3) | d3>7, 2, 1, missing = 1)) %>% 
      mutate(d2cln = if_else(d1cln==1 & d1>0 & d2>0, 1, 2, missing = 2)) %>% 
      mutate(d4cln = if_else(d3cln==1 & d3>0 & d4>0, 1, 2, missing = 2)) %>% 
      mutate(d2cln = replace(d2cln, d1==0 & d2==0, 1)) %>% 
      mutate(d4cln = replace(d4cln, d3==0 & d4==0, 1)) %>% 
      mutate(fruitcln = if_else(d1cln==1 & d2cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(vegcln = if_else(d3cln==1 & d4cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(amount = ifelse(fruitcln==1 & vegcln==1, ((d1*d2)/7 + (d3*d4)/7), NA)) %>% 
      mutate(amount = ifelse(fruitcln==1 & vegcln==2, (d1*d2)/7, amount)) %>% 
      mutate(amount = ifelse(fruitcln==2 & vegcln==1, (d3*d4)/7, amount)) %>% 
      mutate(c = if_else(amount>=5, "2) 5 or more servings of fruit/veg on avg per day", 
                         "1) <5 servings of fruit/veg on avg per day", 
                         missing = "1) <5 servings of fruit/veg on avg per day" )) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(amount<1, "0 servings of fruit/veg per day", NA)) %>% 
      mutate(d = ifelse(amount>=1 & amount<3, "1-2 servings of fruit/veg on avg per day", d)) %>% 
      mutate(d = ifelse(amount>=3 & amount<5, "3-4 servings of fruit/veg on avg per day", d)) %>% 
      mutate(d = ifelse(amount>=5, "5 or more servings of fruit/veg on avg per day", d)) %>% 
      mutate(d = factor(d)) %>% 
      mutate(cln = if_else(valid==1 & (fruitcln==1 | vegcln==1), 1, 2, missing = 2))
  }

}
