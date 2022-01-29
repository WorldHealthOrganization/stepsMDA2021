################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# MODDED FOR MDA

# Variables used in Odentures
# AgeRange Sex Valid O5 O6a O6b Stratum PSU WStep1 agerange2 UR Region

odentures <- function(.data) {
  
  # variable names that are used in the function
  odentures_names <- c("sex", "o5", "o6a", "o6b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, odentures_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(odentures_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(cln = if_else((o5==1 | o5==2) & valid==1, 1, 2, missing = 2)) %>%
      mutate(o6acln = if_else(o5==1 & (o6a==1 | o6a==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(o6bcln = if_else(o5==1 & (o6b==1 | o6b==2) & valid==1, 1, 2, missing = 2)) %>% 
      mutate(c = ifelse(o5==1, "1) has dentures", NA)) %>% 
      mutate(c = replace(c, o5==2, "2) does not have dentures")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(d = ifelse(o6a==1, "1) has upper dentures", NA)) %>% 
      mutate(d = replace(d, o6a==2, "2) does not have upper dentures")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(e = ifelse(o6b==1, "1) has lower dentures", NA)) %>% 
      mutate(e = replace(e, o6b==2, "2) does not have lower dentures")) %>% 
      mutate(e = factor(e)) %>% 
      mutate(f = ifelse(o6a==1 & o6b==1, "1) has both upper and lower dentures", 
                        "2) does not have both upper and lower dentures")) %>% 
      mutate(f = factor(f)) 
  }
  
}





# # Variables used in Odentures
# # AgeRange Sex Valid O4 O5a O5b Stratum PSU WStep1 agerange2 UR Region
# 
# #   "qr", "agerange", "agerange2", "sex", "valid", "ur", "region",
# #   "o4", "o5a", "o5b", 
# #   "psu", "stratum", "wstep1"
# 
# 
# 
# odentures <- function(.data) {
# 
#   # variable names that are used in the function
#   odentures_names <- c("sex", "o4", "o5a", "o5b")
#   
#   # check which names are not in the data before proceeding
#   if(!all(i <- rlang::has_name(.data, odentures_names))) {
#     stop(sprintf(
#       "%s doesn't contain: %s",
#       deparse(substitute(.data)),
#       paste(odentures_names[!i], collapse=", ")))
#   } else {
#     .data %>%
#       mutate(cln = if_else((o4==1 | o4==2) & valid==1, 1, 2, missing = 2)) %>%
#       mutate(o5acln = if_else(o4==1 & (o5a==1 | o5a==2) & valid==1, 1, 2, missing = 2)) %>% 
#       mutate(o5bcln = if_else(o4==1 & (o5b==1 | o5b==2) & valid==1, 1, 2, missing = 2)) %>% 
#       mutate(c = ifelse(o4==1, "1) has dentures", NA)) %>% 
#       mutate(c = replace(c, o4==2, "2) does not have dentures")) %>% 
#       mutate(c = factor(c)) %>% 
#       mutate(d = ifelse(o5a==1, "1) has upper dentures", NA)) %>% 
#       mutate(d = replace(d, o5a==2, "2) does not have upper dentures")) %>% 
#       mutate(d = factor(d)) %>% 
#       mutate(e = ifelse(o5b==1, "1) has lower dentures", NA)) %>% 
#       mutate(e = replace(e, o5b==2, "2) does not have lower dentures")) %>% 
#       mutate(e = factor(e)) %>% 
#       mutate(f = ifelse(o5a==1 & o5b==1, "1) has both upper and lower dentures", 
#                         "2) does not have both upper and lower dentures")) %>% 
#       mutate(f = factor(f)) 
#   }
# 
# }
