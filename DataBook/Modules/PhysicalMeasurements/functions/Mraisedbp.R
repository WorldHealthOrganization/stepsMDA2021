################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Mraisedbp
# AgeRange Sex Valid M4a M4b M5a M5b M6a M6b M7 H1 H2a H3 PSU Stratum WStep2 agerange2 UR Region

mraisedbp <- function(.data) {

  # variable names that are used in the function
  mraisedbp_names <- c("sex", "m4a", "m4b", "m5a", "m5b", "m6a", "m6b", "m7", "h1", "h2a", "h3")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, mraisedbp_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(mraisedbp_names[!i], collapse=", ")))
  } else {
    .data %>%
      mutate(treatment = if_else(h3==1 | m7==1, 1, 2, missing = 2)) %>% 
      mutate(m4acln = if_else(m4a>=40 & m4a<=300, 1, 2, missing = 2)) %>% 
      mutate(m5acln = if_else(m5a>=40 & m5a<=300, 1, 2, missing = 2)) %>% 
      mutate(m6acln = if_else(m6a>=40 & m6a<=300, 1, 2, missing = 2)) %>% 
      mutate(m4bcln = if_else(m4b>=30 & m4b<=200, 1, 2, missing = 2)) %>% 
      mutate(m5bcln = if_else(m5b>=30 & m5b<=200, 1, 2, missing = 2)) %>% 
      mutate(m6bcln = if_else(m6b>=30 & m6b<=200, 1, 2, missing = 2)) %>% 
      ###
      mutate(sbp = ifelse((m4acln==1 | m4acln==2) & m5acln==1 & m6acln==1, (m5a+m6a)/2, NA)) %>% 
      mutate(sbp = ifelse(m4acln==1 & m5acln==2 & m6acln==1, (m4a+m6a)/2, sbp)) %>% 
      mutate(sbp = ifelse(m4acln==1 & m5acln==1 & m6acln==2, (m4a+m5a)/2, sbp)) %>% 
      mutate(dbp = ifelse((m4bcln==1 | m4bcln==2) & m5bcln==1 & m6bcln==1, (m5b+m6b)/2, NA)) %>% 
      mutate(dbp = ifelse(m4bcln==1 & m5bcln==2 & m6bcln==1, (m4b+m6b)/2, dbp)) %>% 
      mutate(dbp = ifelse(m4bcln==1 & m5bcln==1 & m6bcln==2, (m4b+m5b)/2, dbp)) %>% 
      mutate(sbpcln = if_else(sbp>=40 & sbp<=300 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(dbpcln = if_else(dbp>=30 & dbp<=200 & valid==1, 1, 2, missing = 2)) %>% 
      mutate(cln = if_else(sbpcln==1 & dbpcln==1 & valid==1, 1, 2)) %>% 
      mutate(cln = replace(cln, (m7==1 | h3==1) & (h2a==2 | is.na(h2a)), 2)) %>% 
      ###
      mutate(clnnomeds = if_else(cln==1 & treatment==2, 1, 2, missing = 2)) %>% 
      mutate(raisedsbp = ifelse(sbp<140, 1, NA)) %>% 
      mutate(raisedsbp = ifelse(sbp>=140 & sbp<160, 2, raisedsbp)) %>% 
      mutate(raisedsbp = ifelse(sbp>=160, 3, raisedsbp)) %>% 
      mutate(raiseddbp = ifelse(dbp<90, 1, NA)) %>% 
      mutate(raiseddbp = ifelse(dbp>=90 & dbp<100, 2, raiseddbp)) %>% 
      mutate(raiseddbp = ifelse(dbp>=100, 3, raiseddbp)) %>% 
      mutate(raisedbp_140_90 = if_else((raisedsbp>=2 | raiseddbp>=2), "1)SBP>=140 and/or DBP>=90", 
                                       "2)SBP<140 and DBP<90", missing = "2)SBP<140 and DBP<90")) %>% 
      mutate(raisedbp_140_90 = factor(raisedbp_140_90)) %>% 
      mutate(raisedbp_160_100 = if_else((raisedsbp==3 | raiseddbp==3), "1)SBP>=160 and/or DBP>=100", 
                                        "2)SBP<160 and DBP<100", missing = "2)SBP<160 and DBP<100")) %>% 
      mutate(raisedbp_160_100 = factor(raisedbp_160_100)) %>% 
      mutate(raisedbp_140_90_or_meds = if_else(raisedsbp>=2 | raiseddbp>=2 | treatment==1, 
                                               "1)SBP>=140 and/or DBP>=90 or on meds", 
                                               "2)SBP<140 and DBP<90 and not on meds", 
                                               missing = "2)SBP<140 and DBP<90 and not on meds")) %>% 
      mutate(raisedbp_140_90_or_meds = factor(raisedbp_140_90_or_meds)) %>% 
      mutate(raisedbp_160_100_or_meds = if_else(raisedsbp==3 | raiseddbp==3 | treatment==1, 
                                                "1)SBP>=160 and/or DBP>=100 or on meds", 
                                                "2)SBP<160 and DBP<100 and not on meds", 
                                                missing = "2)SBP<160 and DBP<100 and not on meds")) %>% 
      mutate(raisedbp_160_100_or_meds = factor(raisedbp_160_100_or_meds)) %>% 
      ###
      mutate(htn_control_cln = if_else(cln==1 & raisedbp_140_90_or_meds=="1)SBP>=140 and/or DBP>=90 or on meds", 
                                       1, 2, missing = 2)) %>% 
      mutate(htn_control = ifelse(htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & 
                                    (h1==2 | h2a==2), "1) Not previously diagnosed", NA)) %>% 
      mutate(htn_control = replace(htn_control, htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & 
                                     h2a==1 & treatment==2, "2) Previously diagnosed, not on meds")) %>%
      mutate(htn_control = replace(htn_control, htn_control_cln==1 & (raisedsbp>=2 | raiseddbp>=2) & 
                                     h2a==1 & treatment==1, "3) Previously diagnosed, on meds, not controlled")) %>% 
      mutate(htn_control = replace(htn_control, htn_control_cln==1 & raisedsbp==1 & raiseddbp==1 & 
                                     h2a==1 & treatment==1, "4) Previously diagnosed, on meds, controlled")) %>% 
      mutate(htn_control = factor(htn_control)) %>% 
      ###
      mutate(bp_control_old = ifelse(raisedsbp==1 & raiseddbp==1 & treatment==1, 
                                     "1) on meds and BP not raised", NA)) %>% 
      mutate(bp_control_old = replace(bp_control_old, (raisedsbp==2 | raisedsbp==3 | raiseddbp==2 | raiseddbp==3) & 
                                        treatment==1, "2) on meds and BP raised")) %>% 
      mutate(bp_control_old = replace(bp_control_old, (raisedsbp==2 | raisedsbp==3 | raiseddbp==2 | raiseddbp==3) & 
                                        treatment==2, "3) not on meds and BP raised")) %>% 
      mutate(bp_control_old = factor(bp_control_old))
  }

}
