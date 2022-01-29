################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

cleanrecodep1p15 <- function(.data) {
  
  # variable names that are used in the function
  cleanrecodep1p15_names <- c("p1", "p2", "p3a", "p3b", "p4", "p5", "p6a", "p6b", 
                         "p7", "p8", "p9a", "p9b", "p10", "p11", "p12a", "p12b", 
                         "p13", "p14", "p15a", "p15b")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cleanrecodep1p15_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cleanrecodep1p15_names[!i], collapse=", ")))
  } else {
    .data %>%
      ### Clean Recode P1-P3
      mutate(p3a = replace(p3a, p3a==15 & (is.na(p3b) | p3b==0 | p3b==15 | p3b==77 | p3b==88 | p3b==99), 0)) %>% 
      mutate(p3b = replace(p3b, p3a==15 & (is.na(p3b) | p3b==0 | p3b==15 | p3b==77 | p3b==88 | p3b==99), 15)) %>% 
      mutate(p3a = replace(p3a, p3a==30 & (is.na(p3b) | p3b==0 | p3b==30 | p3b==77 | p3b==88 | p3b==99), 0)) %>% 
      mutate(p3b = replace(p3b, p3a==30 & (is.na(p3b) | p3b==0 | p3b==30 | p3b==77 | p3b==88 | p3b==99), 15)) %>% 
      mutate(p3a = replace(p3a, p3a==45 & (is.na(p3b) | p3b==0 | p3b==45 | p3b==77 | p3b==88 | p3b==99), 0)) %>% 
      mutate(p3b = replace(p3b, p3a==45 & (is.na(p3b) | p3b==0 | p3b==45 | p3b==77 | p3b==88 | p3b==99), 45)) %>% 
      mutate(p3a = replace(p3a, p3a==60 & (is.na(p3b) | p3b==0 | p3b==60 | p3b==77 | p3b==88 | p3b==99), 1)) %>% 
      mutate(p3b = replace(p3b, p3a==60 & (is.na(p3b) | p3b==0 | p3b==60 | p3b==77 | p3b==88 | p3b==99), 0)) %>%
      mutate(p3a = replace(p3a, (p3a==7 & p3b==77) | (p3a==8 & p3b==88) | (p3a==9 & p3b==99), 0)) %>% 
      mutate(p3b = replace(p3b, (p3a==7 & p3b==77) | (p3a==8 & p3b==88) | (p3a==9 & p3b==99), 0)) %>% 
      mutate(p3b = replace(p3b, p3b==77 | p3b==88 | p3b==99, 0)) %>% 
      mutate(p3a = replace(p3a, p3a==77 | p3a==88 | p3a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p3amin = ifelse(is.na(p3a), 0, p3a*60)) %>% 
      mutate(p3bmin = ifelse(is.na(p3b), 0, p3b)) %>% 
      mutate(p3 = p3amin + p3bmin) %>% 
      # Cleans P1-P3
      mutate(p2 = replace(p2, is.na(p2) | p2==99, 0)) %>% 
      mutate(p2cln = if_else((p1==1 & p2>0 & p2<8) | (p1==2 & p2==0), 1, 2, missing = 2)) %>% 
      mutate(p3cln = if_else(p2cln==1 & p2>0 & p2<8 & p3>9 & p3<961, 1, 2, missing = 2)) %>% 
      mutate(p3cln = replace(p3cln, p2cln==1 & p2==0 & p3==0, 1)) %>% 
      mutate(p1t3cln = if_else(p3cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(p1t3cln = replace(p1t3cln, is.na(p1) & p2==0 & p3==0 & valid==1, 1)) %>% 
      ### Clean Recode P4-P6
      mutate(p6a = replace(p6a, p6a==15 & (is.na(p6b) | p6b==0 | p6b==15 | p6b==77 | p6b==88 | p6b==99), 0)) %>% 
      mutate(p6b = replace(p6b, p6a==15 & (is.na(p6b) | p6b==0 | p6b==15 | p6b==77 | p6b==88 | p6b==99), 15)) %>% 
      mutate(p6a = replace(p6a, p6a==30 & (is.na(p6b) | p6b==0 | p6b==30 | p6b==77 | p6b==88 | p6b==99), 0)) %>% 
      mutate(p6b = replace(p6b, p6a==30 & (is.na(p6b) | p6b==0 | p6b==30 | p6b==77 | p6b==88 | p6b==99), 30)) %>% 
      mutate(p6a = replace(p6a, p6a==45 & (is.na(p6b) | p6b==0 | p6b==45 | p6b==77 | p6b==88 | p6b==99), 0)) %>% 
      mutate(p6b = replace(p6b, p6a==45 & (is.na(p6b) | p6b==0 | p6b==45 | p6b==77 | p6b==88 | p6b==99), 45)) %>% 
      mutate(p6a = replace(p6a, p6a==60 & (is.na(p6b) | p6b==0 | p6b==60 | p6b==77 | p6b==88 | p6b==99), 1)) %>% 
      mutate(p6b = replace(p6b, p6a==60 & (is.na(p6b) | p6b==0 | p6b==60 | p6b==77 | p6b==88 | p6b==99), 0)) %>% 
      mutate(p6a = replace(p6a, (p6a==7 & p6b==77) | (p6a==8 & p6b==88) | (p6a==9 & p6b==99), 0)) %>% 
      mutate(p6b = replace(p6b, (p6a==7 & p6b==77) | (p6a==8 & p6b==88) | (p6a==9 & p6b==99), 0)) %>% 
      mutate(p6b = replace(p6b, p6b==77 | p6b==88 | p6b==99, 0)) %>% 
      mutate(p6a = replace(p6a, p6a==77 | p6a==88 | p6a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p6amin = ifelse(is.na(p6a), 0, p6a*60)) %>% 
      mutate(p6bmin = ifelse(is.na(p6b), 0, p6b)) %>% 
      mutate(p6 = p6amin + p6bmin) %>% 
      # Cleans P4-P6
      mutate(p5 = replace(p5, is.na(p5) | p5==99, 0)) %>% 
      mutate(p5cln = if_else((p4==1 & p5>0 & p5<8) | (p4==2 & p5==0), 1, 2, missing = 2)) %>% 
      mutate(p6cln = if_else(p5cln==1 & p5>0 & p5<8 & p6>9 & p6<961, 1, 2, missing = 2)) %>% 
      mutate(p6cln = replace(p6cln, p5cln==1 & p5==0 & p6==0, 1)) %>% 
      mutate(p4t6cln = if_else(p6cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(p4t6cln = replace(p4t6cln, is.na(p4) & p5==0 & p6==0 & valid==1, 1)) %>% 
      ### Clean Recode P7-P9
      mutate(p9a = replace(p9a, p9a==15 & (is.na(p9b) | p9b==0 | p9b==15 | p9b==77 | p9b==88 | p9b==99), 0)) %>% 
      mutate(p9b = replace(p9b, p9a==15 & (is.na(p9b) | p9b==0 | p9b==15 | p9b==77 | p9b==88 | p9b==99), 15)) %>% 
      mutate(p9a = replace(p9a, p9a==30 & (is.na(p9b) | p9b==0 | p9b==30 | p9b==77 | p9b==88 | p9b==99), 0)) %>% 
      mutate(p9b = replace(p9b, p9a==30 & (is.na(p9b) | p9b==0 | p9b==30 | p9b==77 | p9b==88 | p9b==99), 30)) %>% 
      mutate(p9a = replace(p9a, p9a==45 & (is.na(p9b) | p9b==0 | p9b==45 | p9b==77 | p9b==88 | p9b==99), 0)) %>% 
      mutate(p9b = replace(p9b, p9a==45 & (is.na(p9b) | p9b==0 | p9b==45 | p9b==77 | p9b==88 | p9b==99), 45)) %>% 
      mutate(p9a = replace(p9a, p9a==60 & (is.na(p9b) | p9b==0 | p9b==60 | p9b==77 | p9b==88 | p9b==99), 1)) %>% 
      mutate(p9b = replace(p9b, p9a==60 & (is.na(p9b) | p9b==0 | p9b==60 | p9b==77 | p9b==88 | p9b==99), 0)) %>% 
      mutate(p9a = replace(p9a, (p9a==7 & p9b==77) | (p9a==8 & p9b==88) | (p9a==9 & p9b==99), 0)) %>% 
      mutate(p9b = replace(p9b, (p9a==7 & p9b==77) | (p9a==8 & p9b==88) | (p9a==9 & p9b==99), 0)) %>% 
      mutate(p9b = replace(p9b, p9b==77 | p9b==88 | p9b==99, 0)) %>% 
      mutate(p9a = replace(p9a, p9a==77 | p9a==88 | p9a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p9amin = ifelse(is.na(p9a), 0, p9a*60)) %>% 
      mutate(p9bmin = ifelse(is.na(p9b), 0, p9b)) %>% 
      mutate(p9 = p9amin + p9bmin) %>% 
      # Cleans p7-p9
      mutate(p8 = replace(p8, is.na(p8) | p8==99, 0)) %>% 
      mutate(p8cln = if_else((p7==1 & p8>0 & p8<8) | (p7==2 & p8==0), 1, 2, missing = 2)) %>% 
      mutate(p9cln = if_else(p8cln==1 & p8>0 & p8<8 & p9>9 & p9<961, 1, 2, missing = 2)) %>% 
      mutate(p9cln = replace(p9cln, p8cln==1 & p8==0 & p9==0, 1)) %>% 
      mutate(p7t9cln = if_else(p9cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(p7t9cln = replace(p7t9cln, is.na(p7) & p8==0 & p9==0 & valid==1, 1)) %>% 
      ### Clean Recode P10-P12
      mutate(p12a = replace(p12a, p12a==15 & (is.na(p12b) | p12b==0 | p12b==15 | p12b==77 | p12b==88 | p12b==99), 0)) %>% 
      mutate(p12b = replace(p12b, p12a==15 & (is.na(p12b) | p12b==0 | p12b==15 | p12b==77 | p12b==88 | p12b==99), 15)) %>% 
      mutate(p12a = replace(p12a, p12a==30 & (is.na(p12b) | p12b==0 | p12b==30 | p12b==77 | p12b==88 | p12b==99), 0)) %>% 
      mutate(p12b = replace(p12b, p12a==30 & (is.na(p12b) | p12b==0 | p12b==30 | p12b==77 | p12b==88 | p12b==99), 30)) %>% 
      mutate(p12a = replace(p12a, p12a==45 & (is.na(p12b) | p12b==0 | p12b==45 | p12b==77 | p12b==88 | p12b==99), 0)) %>%
      mutate(p12b = replace(p12b, p12a==45 & (is.na(p12b) | p12b==0 | p12b==45 | p12b==77 | p12b==88 | p12b==99), 45)) %>%
      mutate(p12a = replace(p12a, p12a==60 & (is.na(p12b) | p12b==0 | p12b==60 | p12b==77 | p12b==88 | p12b==99), 1)) %>%
      mutate(p12b = replace(p12b, p12a==60 & (is.na(p12b) | p12b==0 | p12b==60 | p12b==77 | p12b==88 | p12b==99), 0)) %>%
      mutate(p12a = replace(p12a, (p12a==7 & p12b==77) | (p12a==8 & p12b==88) | (p12a==9 & p12b==99), 0)) %>% 
      mutate(p12b = replace(p12b, (p12a==7 & p12b==77) | (p12a==8 & p12b==88) | (p12a==9 & p12b==99), 0)) %>% 
      mutate(p12b = replace(p12b, p12b==77 | p12b==88 | p12b==99, 0)) %>% 
      mutate(p12a = replace(p12a, p12a==77 | p12a==88 | p12a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p12amin = ifelse(is.na(p12a), 0, p12a*60)) %>% 
      mutate(p12bmin = ifelse(is.na(p12b), 0, p12b)) %>% 
      mutate(p12 = p12amin + p12bmin) %>% 
      # Cleans p10-p12
      mutate(p11 = replace(p11, is.na(p11) | p11==99, 0)) %>% 
      mutate(p11cln = if_else((p10==1 & p11>0 & p11<8) | (p10==2 & p11==0), 1, 2, missing = 2)) %>% 
      mutate(p12cln = if_else(p11cln==1 & p11>0 & p11<8 & p12>9 & p12<961, 1, 2, missing = 2)) %>% 
      mutate(p12cln = replace(p12cln, p11cln==1 & p11==0 & p12==0, 1)) %>% 
      mutate(p10t12cln = if_else(p12cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(p10t12cln = replace(p10t12cln, is.na(p10) & p11==0 & p12==0 & valid==1, 1)) %>% 
      ### Clean Recode P13-P15
      mutate(p15a = replace(p15a, p15a==15 & (is.na(p15b) | p15b==0 | p15b==15 | p15b==77 | p15b==88 | p15b==99), 0)) %>% 
      mutate(p15b = replace(p15b, p15a==15 & (is.na(p15b) | p15b==0 | p15b==15 | p15b==77 | p15b==88 | p15b==99), 15)) %>% 
      mutate(p15a = replace(p15a, p15a==30 & (is.na(p15b) | p15b==0 | p15b==30 | p15b==77 | p15b==88 | p15b==99), 0)) %>% 
      mutate(p15b = replace(p15b, p15a==30 & (is.na(p15b) | p15b==0 | p15b==30 | p15b==77 | p15b==88 | p15b==99), 30)) %>% 
      mutate(p15a = replace(p15a, p15a==45 & (is.na(p15b) | p15b==0 | p15b==45 | p15b==77 | p15b==88 | p15b==99), 0)) %>% 
      mutate(p15b = replace(p15b, p15a==45 & (is.na(p15b) | p15b==0 | p15b==45 | p15b==77 | p15b==88 | p15b==99), 45)) %>% 
      mutate(p15a = replace(p15a, p15a==60 & (is.na(p15b) | p15b==0 | p15b==60 | p15b==77 | p15b==88 | p15b==99), 1)) %>% 
      mutate(p15b = replace(p15b, p15a==60 & (is.na(p15b) | p15b==0 | p15b==60 | p15b==77 | p15b==88 | p15b==99), 0)) %>%
      mutate(p15a = replace(p15a, (p15a==7 & p15b==77) | (p15a==8 & p15b==88) | (p15a==9 & p15b==99), 0)) %>% 
      mutate(p15b = replace(p15b, (p15a==7 & p15b==77) | (p15a==8 & p15b==88) | (p15a==9 & p15b==99), 0)) %>%
      mutate(p15b = replace(p15b, p15b==77 | p15b==88 | p15b==99, 0)) %>% 
      mutate(p15a = replace(p15a, p15a==77 | p15a==88 | p15a==99, 0)) %>% 
      # Recode variables into minutes only
      mutate(p15amin = ifelse(is.na(p15a), 0, p15a*60)) %>% 
      mutate(p15bmin = ifelse(is.na(p15b), 0, p15b)) %>% 
      mutate(p15 = p15amin + p15bmin) %>% 
      # Cleans p13-p15
      mutate(p14 = replace(p14, is.na(p14) | p14==99, 0)) %>% 
      mutate(p14cln = if_else((p13==1 & p14>0 & p14<8) | (p13==2 & p14==0), 1, 2, missing = 2)) %>% 
      mutate(p15cln = if_else(p14cln==1 & p14>0 & p14<8 & p15>9 & p15<961, 1, 2, missing = 2)) %>% 
      mutate(p15cln = replace(p15cln, p14cln==1 & p14==0 & p15==0, 1)) %>% 
      mutate(p13t15cln = if_else(p15cln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(p13t15cln = replace(p13t15cln, is.na(p13) & p14==0 & p15==0 & valid==1, 1))
  }
  
}

