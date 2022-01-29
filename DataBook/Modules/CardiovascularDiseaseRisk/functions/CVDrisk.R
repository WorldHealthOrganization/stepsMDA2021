################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in CVDrisk
# C1 Age AgeRange Sex Valid H6 H7a H17 M4a M5a M6a T1 T8 H3 H8 H9 H14 H18 H19 
# H20a H20b H20c H20d H20e H20f B1 B5 B6 B8 PSU Stratum WStep3 agerange2 UR Region

cvdrisk <- function(.data) {

  # variable names that are used in the function
  cvdrisk_names <- c("sex", "c1", "h6", "h7a", "h17", "m4a", "m5a", "m6a", "t1", 
                     "t8", "h3", "h8", "h9", "h14", "h18", "h19", "h20a", "h20b", 
                     "h20c", "h20d", "h20e", "h20f", "b1", "b5", "b6", "b8")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, cvdrisk_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(cvdrisk_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(t10 = NA) %>% 
      mutate(t11 = 10) %>% 
      mutate(t11type = "years") %>% 
      mutate(smokecln = if_else(t1==1 | t1==2, 1, 2, missing = 2)) %>% 
      mutate(smokecln = replace(smokecln, t1==2 & is.na(t8), 2)) %>% 
      mutate(smoker = if_else(t1==1, 1, 2, missing = 2)) %>% 
      # Calculate T11b (months) AND T11c (weeks) into years 
      mutate(t11a = ifelse(t11type>="years", t11, NA)) %>% 
      mutate(t11byrs = ifelse(t11type>="months", t11/12, NA)) %>% 
      mutate(t11cyrs = ifelse(t11type>="weeks", t11/52, NA)) %>% 
      mutate(t10cln = if_else(t10>=7 & t10<=69, 1, 2, missing = 2)) %>% 
      mutate(t11acln = if_else(t11a>0 & t11a<=61, 1, 2, missing = 2)) %>% 
      mutate(t11bcln = if_else(is.na(t11byrs), 2, 1, missing = 1)) %>% 
      mutate(t11ccln = if_else(is.na(t11cyrs), 2, 1, missing = 1)) %>% 
      # Order the priority of responses so that T10 takes precedence over other responses
      mutate(stop = ifelse(t11ccln==1, t11cyrs, NA)) %>% 
      mutate(stop = replace(stop, t11bcln==1, t11byrs)) %>% 
      mutate(stop = replace(stop, t11acln==1, t11a)) %>% 
      mutate(stop = replace(stop, t10cln==1, age-t10)) %>% 
      mutate(smokecln = replace(smokecln, t10cln==2 & t11acln==2 & t11bcln==2 & t11ccln==2 & t1==2 & t8==1, 2)) %>% 
      mutate(smokecln = replace(smokecln, t1==2 & t8==1 & (is.na(stop) | stop>61 | stop<0), 2)) %>% 
      mutate(smoker = replace(smoker, stop<=1, 1)) %>% 
      mutate(m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2)) %>% 
      mutate(m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2)) %>% 
      mutate(m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2)) %>% 
      mutate(sbp = ifelse((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1, (m5a + m6a)/2, NA)) %>%
      mutate(sbp = ifelse(m4acln==1 & m5acln==2 & m6acln==1, (m4a + m6a)/2, sbp)) %>% 
      mutate(sbp = ifelse(m4acln==1 & m5acln==1 & m6acln==2, (m4a + m5a)/2, sbp)) %>% 
      mutate(sbpcln = if_else((sbp<40 | sbp>300) | is.na(sbp), 2, 1, missing = 1)) %>% 
      mutate(chol = ifelse(b8>=2 & b8<=12, b8, NA)) %>% 
      mutate(cholcln = if_else(b8>=2 & b8<=12, 1, 2, missing = 2)) %>% 
      mutate(diabcln = if_else(b5>=1 & b5<=35 & b1==2, 1, 2, missing = 2)) %>% 
      mutate(diab = if_else(b5>=6.1 | b6==1, 1, 2, missing = 2)) %>% 
      mutate(diabcln = replace(diabcln, h6==1 & h7a==1, 1)) %>% 
      mutate(diab = replace(diab, h6==1 & h7a==1, 1)) %>% 
      mutate(cln = if_else(diabcln==1 & cholcln==1 & sbpcln==1 & smokecln==1 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(highrisk = 2) %>% 
      # MALES WITH DIABETES
      mutate(highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=6 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=8 & sbp>=120) | (chol>=7 & sbp>=140)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=5 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=6 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==2 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      # FEMALES WITH DIABETES
      mutate(highrisk = replace(highrisk, c1==2 & (age>=40 & age<60) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==2 & (age>=60 & age<70) & smoker==1 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=6 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==2 & (age>=40 & age<60) & smoker==2 & diab==1 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==2 & (age>=60 & age<70) & smoker==2 & diab==1 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      # MEN WITHOUT DIABETES
      mutate(highrisk = replace(highrisk, c1==1 & (age>=40 & age<50) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=6 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=50 & age<60) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==1 & diab==2 & ((chol>=8 & sbp>=140) | (chol>=7 & sbp>=160) | (chol>=4 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=40 & age<60) & smoker==2 & diab==2 & (chol>=7 & sbp>=180), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==1 & (age>=60 & age<70) & smoker==2 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1)) %>% 
      # WOMEN WITHOUT DIABETES
      mutate(highrisk = replace(highrisk, c1==2 & (age>=40 & age<70) & smoker==1 & diab==2 & ((chol>=8 & sbp>=160) | (chol>=5 & sbp>=180)), 1)) %>% 
      mutate(highrisk = replace(highrisk, c1==2 & (age>=40 & age<70) & smoker==2 & diab==2 & (chol>=7 & sbp>=180), 1)) %>% 
      mutate(highrisk = replace(highrisk, h17==1, 1)) %>% 
      ###
      mutate(c = ifelse(highrisk==1, "risk 30% or more or has CVD", NA)) %>% 
      mutate(c = replace(c, highrisk==2, "risk less than 30%")) %>% 
      mutate(c = factor(c)) %>% 
      mutate(drugs = if_else(h3==1 | h8==1 | h9==1 | h14==1 | h18==1 | h19==1, 1, 2, missing = 2)) %>% 
      mutate(counseling = if_else(h20a==1 | h20b==1 | h20c==1 | h20d==1 | h20e==1 | h20f==1, 1, 2, missing = 2)) %>% 
      mutate(d = if_else(drugs==1 & counseling==1, "1) received drug therapy and counseling", 
                         "2) did not receive drug therapy and counseling", missing = "2) did not receive drug therapy and counseling")) %>% 
      mutate(d = factor(d)) %>% 
      mutate(agerangecvd = ifelse(age>=40 & age<=54, "40–54", NA)) %>% 
      mutate(agerangecvd = replace(agerangecvd, age>=55 & age<=69, "55–69")) %>% 
      mutate(agerangecvd = factor(agerangecvd)) %>% 
      mutate(cln = replace(cln, age<40, 2))
  }

}
