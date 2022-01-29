################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Variables used in Aunrecpastweektype
# AgeRange Sex Valid A1 A2 A5 A10a A10b A10c A10d A10e A10f A10g A11 A12a A12b 
# A12c A12d A12e PSU Stratum WStep1 agerange2 UR Region

aunrecpastweektype <- function(.data) {
  
  aunrecpastweektype_names <- c("sex", "a1", "a2", "a5", "a10a", "a10b", "a10c", 
                                "a10d", "a10e", "a10f", "a10g", "a11", "a12a", 
                                "a12b", "a12c", "a12d", "a12e")
  
  # check which names are not in the data before proceeding
  if(!all(i <- rlang::has_name(.data, aunrecpastweektype_names))) {
    stop(sprintf(
      "%s doesn't contain: %s",
      deparse(substitute(.data)),
      paste(aunrecpastweektype_names[!i], collapse=", ")))
  } else {
    .data %>% 
      mutate(allmissing = 2) %>% 
      mutate(across(c(a10a, a10b, a10c, a10d, a10e, a10f, a10g), ~na_if(., 77))) %>% 
      mutate(allmissing = replace(allmissing, is.na(a10a) & is.na(a10b) & is.na(a10c) & 
                                    is.na(a10d) & is.na(a10e) & is.na(a10f) & is.na(a10g), 1)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10a), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10b), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10c), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10d), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10e), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10f), 0)) %>% 
      mutate(a10a = replace(a10a, allmissing==2 & is.na(a10g), 0)) %>% 
      mutate(a10cln = 2) %>% 
      mutate(a10cln = replace(a10cln, a10a<51 & a10b<51 & a10c<51 & a10d<51 & a10e<51 & a10f<51 & a10g<51, 1)) %>%
      mutate(past7total = a10a + a10b + a10c + a10d + a10e + a10f + a10g) %>% 
      mutate(cln = 2) %>% 
      mutate(cln = replace(cln, a1==1 & a2==1 & a5==1 & a10cln==1 & valid==1 & past7total>0, 1)) %>% 
      mutate(across(c(a12a, a12b, a12c, a12d, a12e), ~na_if(., 77))) %>% 
      mutate(a12a = replace(a12a, (a11==1 & is.na(a12a)) | a11==2, 0)) %>% 
      mutate(a12b = replace(a12b, (a11==1 & is.na(a12b)) | a11==2, 0)) %>% 
      mutate(a12c = replace(a12c, (a11==1 & is.na(a12c)) | a11==2, 0)) %>% 
      mutate(a12d = replace(a12d, (a11==1 & is.na(a12d)) | a11==2, 0)) %>% 
      mutate(a12e = replace(a12e, (a11==1 & is.na(a12e)) | a11==2, 0)) %>% 
      mutate(past7unrec = a12a + a12b + a12c + a12d + a12e) %>% 
      mutate(cln = replace(cln, past7unrec>past7total & !is.na(past7unrec), 2)) %>% 
      mutate(cln = replace(cln, past7unrec==0, 2)) %>% 
      mutate(cln = replace(cln, a11==2, 2)) %>% 
      filter(cln == 1) %>% 
      mutate(past7unrecwt = past7unrec*wstep1) %>%
      mutate(a12awt = a12a*wstep1) %>% 
      mutate(a12bwt = a12b*wstep1) %>% 
      mutate(a12cwt = a12c*wstep1) %>% 
      mutate(a12dwt = a12d*wstep1) %>% 
      mutate(a12ewt = a12e*wstep1) %>% 
      mutate(percentunrec_a = a12awt/past7unrecwt) %>% 
      mutate(percentunrec_b = a12bwt/past7unrecwt) %>% 
      mutate(percentunrec_c = a12cwt/past7unrecwt) %>% 
      mutate(percentunrec_d = a12dwt/past7unrecwt) %>% 
      mutate(percentunrec_e = a12ewt/past7unrecwt)
  }
  
}

