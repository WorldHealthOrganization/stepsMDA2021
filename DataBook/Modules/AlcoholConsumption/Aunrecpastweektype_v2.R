################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# STATA SCRIPT percent_unrec_alc_type

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aunrecpastweektype.R"))

aunrecpastweektype_df <- aunrecpastweektype(data)

################################################################################

library(srvyr)

# Specifying design
STEPSClean <- aunrecpastweektype_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

#options(survey.lonely.psu="fail") # default setting

#options(survey.lonely.psu="remove")
options(survey.lonely.psu="adjust") # conservative method
# more details here: https://r-survey.r-forge.r-project.org/survey/exmample-lonely.html
################################################################################

# Load functions
source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of unrecorded alcohol from all alcohol consumed during past 7 days

# CALCULATING THE OUTPUT LIKE IN STATA

# 1 - home-brewed spirits

# ALTERNATIVE PACKAGES USAGE (deprecated because of dplyr)
# STATA REPLICA 
# BOTH SEXES
# more details: https://stats.idre.ucla.edu/r/faq/how-can-i-collapse-my-data-in-r/
# library(doBy)
# attach(aunrecpastweektype_df)
# collapse1 <- summaryBy(a12awt + a12bwt + a12cwt + a12dwt + a12ewt + past7unrecwt ~ agerange, FUN=c(sum), data=aunrecpastweektype_df)
# collapse1
# 
# library(collapse)
# collapse2 <- collap(aunrecpastweektype_df, a12awt + a12bwt + a12cwt + a12dwt + a12ewt + past7unrecwt ~ agerange, FUN = list(fsum))
# collapse2

# THIS GETS NUMBERS EXATLY AS IN STATA OUTPUT

aunrecpastweektype_stata_summary <- function(..., .select_agerange, .group_var, .agerange, .total) {
  if(!missing(.select_agerange) & !missing(.group_var) & missing(.agerange) & missing(.total)) {
    aunrecpastweektype_df %>%
      filter(...) %>% 
      select({{ .select_agerange }},a12awt,a12bwt,a12cwt,a12dwt,a12ewt,past7unrecwt) %>% 
      group_by({{ .group_var }}) %>%
      summarise(n = n(), 
                across(everything(), sum)) %>%  
      mutate((across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt)) / past7unrecwt) * 100) %>%
      mutate(across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt), function(x) sprintf("%.1f", x))) %>% 
      select(-past7unrecwt)
  } 
  else if (missing(.select_agerange) & missing(.group_var) & !missing(.agerange) & !missing(.total)) {
    aunrecpastweektype_df %>%
      filter(...) %>% 
      select({{ .select_agerange }},a12awt,a12bwt,a12cwt,a12dwt,a12ewt,past7unrecwt) %>% 
      group_by({{ .group_var }}) %>%
      summarise(n = n(), 
                across(everything(), sum)) %>%  
      mutate((across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt)) / past7unrecwt) * 100) %>% 
      mutate(across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt), function(x) sprintf("%.1f", x))) %>%
      select(-past7unrecwt) %>% mutate("{{ .agerange }}" := .total, .before = 1)
  }
}


# MEN
# example kept for your information:
# aunrecpastweektype_m <- aunrecpastweektype_df %>%
#   filter(sex=="Men") %>% 
#   select(agerange,a12awt,a12bwt,a12cwt,a12dwt,a12ewt,past7unrecwt) %>% 
#   group_by(agerange) %>%
#   summarise(n = n(), 
#             across(everything(), sum)) %>%  
#   mutate((across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt)) / past7unrecwt) * 100) %>% 
#   select(-past7unrecwt)
# aunrecpastweektype_m
# 
# aunrecpastweektype_m_t <- aunrecpastweektype_df %>%
#   filter(sex=="Men") %>% 
#   select(a12awt,a12bwt,a12cwt,a12dwt,a12ewt,past7unrecwt) %>% 
#   summarise(n = n(), 
#             across(everything(), sum)) %>%  
#   mutate((across(c(a12awt,a12bwt,a12cwt,a12dwt,a12ewt)) / past7unrecwt) * 100) %>% 
#   select(-past7unrecwt) %>% mutate(agerange = "18–69", .before = 1)
# aunrecpastweektype_m_t

aunrecpastweektype_m <- aunrecpastweektype_stata_summary(sex=="Men", .select_agerange = agerange, .group_var = agerange)
aunrecpastweektype_m_t <- aunrecpastweektype_stata_summary(sex=="Men", .agerange = agerange, .total = "18–69")
aunrecpastweektype_m_t_j <- rbind(aunrecpastweektype_m, aunrecpastweektype_m_t)
aunrecpastweektype_m_t_j

# MEN - URBAN
aunrecpastweektype_m_u <- aunrecpastweektype_stata_summary(sex=="Men", ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_m_t_u <- aunrecpastweektype_stata_summary(sex=="Men", ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_m_t_u_j <- rbind(aunrecpastweektype_m_u, aunrecpastweektype_m_t_u)
aunrecpastweektype_m_t_u_j

# MEN - RURAL
aunrecpastweektype_m_r <- aunrecpastweektype_stata_summary(sex=="Men", ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_m_t_r <- aunrecpastweektype_stata_summary(sex=="Men", ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_m_t_r_j <- rbind(aunrecpastweektype_m_r, aunrecpastweektype_m_t_r)
aunrecpastweektype_m_t_r_j

# WOMEN
aunrecpastweektype_w <- aunrecpastweektype_stata_summary(sex=="Women", .select_agerange = agerange, .group_var = agerange)
aunrecpastweektype_w_t <- aunrecpastweektype_stata_summary(sex=="Women", .agerange = agerange, .total = "18–69")
aunrecpastweektype_w_t_j <- rbind(aunrecpastweektype_w, aunrecpastweektype_w_t)
aunrecpastweektype_w_t_j

# WOMEN - URBAN
aunrecpastweektype_w_u <- aunrecpastweektype_stata_summary(sex=="Women", ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_w_t_u <- aunrecpastweektype_stata_summary(sex=="Women", ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_w_t_u_j <- rbind(aunrecpastweektype_w_u, aunrecpastweektype_w_t_u)
aunrecpastweektype_w_t_u_j

# WOMEN - RURAL
aunrecpastweektype_w_r <- aunrecpastweektype_stata_summary(sex=="Women", ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_w_t_r <- aunrecpastweektype_stata_summary(sex=="Women", ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_w_t_r_j <- rbind(aunrecpastweektype_w_r, aunrecpastweektype_w_t_r)
aunrecpastweektype_w_t_r_j

# BOTH SEXES
aunrecpastweektype_b <- aunrecpastweektype_stata_summary(.select_agerange = agerange, .group_var = agerange)
aunrecpastweektype_b_t <- aunrecpastweektype_stata_summary(.agerange = agerange, .total = "18–69")
aunrecpastweektype_b_t_j <- rbind(aunrecpastweektype_b, aunrecpastweektype_b_t)
aunrecpastweektype_b_t_j

# BOTH SEXES - URBAN
aunrecpastweektype_b_u <- aunrecpastweektype_stata_summary(ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_b_t_u <- aunrecpastweektype_stata_summary(ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_b_t_u_j <- rbind(aunrecpastweektype_b_u, aunrecpastweektype_b_t_u)
aunrecpastweektype_b_t_u_j

# BOTH SEXES - RURAL
aunrecpastweektype_b_r <- aunrecpastweektype_stata_summary(ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecpastweektype_b_t_r <- aunrecpastweektype_stata_summary(ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecpastweektype_b_t_r_j <- rbind(aunrecpastweektype_b_r, aunrecpastweektype_b_t_r)
aunrecpastweektype_b_t_r_j

# BOTH SEXES - REGION
aunrecpastweektype_b_reg <- aunrecpastweektype_stata_summary(.select_agerange = region, .group_var = region)
aunrecpastweektype_b_t_reg <- aunrecpastweektype_stata_summary(.agerange = region, .total = "Total")
aunrecpastweektype_b_t_reg_j <- rbind(aunrecpastweektype_b_reg, aunrecpastweektype_b_t_reg)
aunrecpastweektype_b_t_reg_j

m_w_b_u_r_reg <- c("m","w","b","m_u","w_u","b_u","m_r","w_r","b_r","b_reg")
aunrecpastweektype <- 
  list(
    # men, women, both
    aunrecpastweektype_m_t_j, aunrecpastweektype_w_t_j, aunrecpastweektype_b_t_j,
    # urban (men, women, both)
    aunrecpastweektype_m_t_u_j, aunrecpastweektype_w_t_u_j, aunrecpastweektype_b_t_u_j,
    # rural (men, women, both)
    aunrecpastweektype_m_t_r_j, aunrecpastweektype_w_t_r_j, aunrecpastweektype_b_t_r_j,
    # region (both only)
    aunrecpastweektype_b_t_reg_j) %>% setNames(m_w_b_u_r_reg)

#################################




