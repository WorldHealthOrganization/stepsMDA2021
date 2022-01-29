################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# STATA SCRIPT percent_unrec_alc

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aunrecordedpastweek.R"))

aunrecordedpastweek_df <- aunrecordedpastweek(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aunrecordedpastweek_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of unrecorded alcohol from all alcohol consumed during past 7 days

# WHAT STATA OUTPUT HAS

# MEN
a_m <- aunrecordedpastweek_df %>%
  select(agerange,past7totalwt,past7unrecwt,sex) %>%
  group_by(agerange,sex) %>%
  summarise(n = n(),
            across(everything(), sum)) %>%
  mutate(past7unrecwt = (past7unrecwt / past7totalwt) * 100) %>%
  filter(sex=="Men") %>% select(-c(past7totalwt,sex))
a_m

# WOMEN
a_w <- aunrecordedpastweek_df %>%
  select(agerange,past7totalwt,past7unrecwt,sex) %>%
  group_by(agerange,sex) %>%
  summarise(n = n(),
            across(everything(), sum)) %>%
  mutate(past7unrecwt = (past7unrecwt / past7totalwt) * 100) %>%
  filter(sex=="Women") %>% select(-c(past7totalwt,sex))
a_w

# BOTH SEXES
a_b <- aunrecordedpastweek_df %>%
  select(agerange,past7totalwt,past7unrecwt) %>%
  group_by(agerange) %>%
  summarise(n = n(),
            across(everything(), sum)) %>%
  mutate(past7unrecwt = (past7unrecwt / past7totalwt) * 100) %>%
  select(-past7totalwt)
a_b

aunrecordedpastweek_stata_summary <- function(..., .select_agerange, .group_var, .agerange, .total) {
  if(!missing(.select_agerange) & !missing(.group_var) & missing(.agerange) & missing(.total)) {
    aunrecordedpastweek_df %>%
      filter(...) %>%
      select({{ .select_agerange }},past7totalwt,past7unrecwt) %>%
      group_by({{ .group_var }}) %>%
      summarise(n = n(),
                across(everything(), sum)) %>%
      mutate(past7unrecwt = (past7unrecwt / past7totalwt) * 100) %>%
      mutate(past7unrecwt = sprintf("%.1f", past7unrecwt)) %>% 
      select(-past7totalwt)
  } 
  else if (missing(.select_agerange) & missing(.group_var) & !missing(.agerange) & !missing(.total)) {
    aunrecordedpastweek_df %>%
      filter(...) %>%
      select({{ .select_agerange }},past7totalwt,past7unrecwt) %>%
      group_by({{ .group_var }}) %>%
      summarise(n = n(),
                across(everything(), sum)) %>%
      mutate(past7unrecwt = (past7unrecwt / past7totalwt) * 100) %>%
      mutate(past7unrecwt = sprintf("%.1f", past7unrecwt)) %>%
      select(-past7totalwt) %>% mutate("{{ .agerange }}" := .total, .before = 1)
  }
}
# MEN
aunrecordedpastweek_m <- aunrecordedpastweek_stata_summary(sex=="Men", .select_agerange = agerange, .group_var = agerange)
aunrecordedpastweek_m_t <- aunrecordedpastweek_stata_summary(sex=="Men", .agerange = agerange, .total = "18–69")
aunrecordedpastweek_m_t_j <- rbind(aunrecordedpastweek_m, aunrecordedpastweek_m_t)
aunrecordedpastweek_m_t_j

# MEN - URBAN
aunrecordedpastweek_m_u <- aunrecordedpastweek_stata_summary(sex=="Men", ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_m_t_u <- aunrecordedpastweek_stata_summary(sex=="Men", ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_m_t_u_j <- rbind(aunrecordedpastweek_m_u, aunrecordedpastweek_m_t_u)
aunrecordedpastweek_m_t_u_j

# MEN - RURAL
aunrecordedpastweek_m_r <- aunrecordedpastweek_stata_summary(sex=="Men", ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_m_t_r <- aunrecordedpastweek_stata_summary(sex=="Men", ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_m_t_r_j <- rbind(aunrecordedpastweek_m_r, aunrecordedpastweek_m_t_r)
aunrecordedpastweek_m_t_r_j

# WOMEN
aunrecordedpastweek_w <- aunrecordedpastweek_stata_summary(sex=="Women", .select_agerange = agerange, .group_var = agerange)
aunrecordedpastweek_w_t <- aunrecordedpastweek_stata_summary(sex=="Women", .agerange = agerange, .total = "18–69")
aunrecordedpastweek_w_t_j <- rbind(aunrecordedpastweek_w, aunrecordedpastweek_w_t)
aunrecordedpastweek_w_t_j

# WOMEN - URBAN
aunrecordedpastweek_w_u <- aunrecordedpastweek_stata_summary(sex=="Women", ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_w_t_u <- aunrecordedpastweek_stata_summary(sex=="Women", ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_w_t_u_j <- rbind(aunrecordedpastweek_w_u, aunrecordedpastweek_w_t_u)
aunrecordedpastweek_w_t_u_j

# WOMEN - RURAL
aunrecordedpastweek_w_r <- aunrecordedpastweek_stata_summary(sex=="Women", ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_w_t_r <- aunrecordedpastweek_stata_summary(sex=="Women", ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_w_t_r_j <- rbind(aunrecordedpastweek_w_r, aunrecordedpastweek_w_t_r)
aunrecordedpastweek_w_t_r_j

# BOTH SEXES
aunrecordedpastweek_b <- aunrecordedpastweek_stata_summary(.select_agerange = agerange, .group_var = agerange)
aunrecordedpastweek_b_t <- aunrecordedpastweek_stata_summary(.agerange = agerange, .total = "18–69")
aunrecordedpastweek_b_t_j <- rbind(aunrecordedpastweek_b, aunrecordedpastweek_b_t)
aunrecordedpastweek_b_t_j

# BOTH SEXES - URBAN
aunrecordedpastweek_b_u <- aunrecordedpastweek_stata_summary(ur=="Urban", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_b_t_u <- aunrecordedpastweek_stata_summary(ur=="Urban", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_b_t_u_j <- rbind(aunrecordedpastweek_b_u, aunrecordedpastweek_b_t_u)
aunrecordedpastweek_b_t_u_j

# BOTH SEXES - RURAL
aunrecordedpastweek_b_r <- aunrecordedpastweek_stata_summary(ur=="Rural", .select_agerange = agerange2, .group_var = agerange2)
aunrecordedpastweek_b_t_r <- aunrecordedpastweek_stata_summary(ur=="Rural", .agerange = agerange2, .total = "18–69")
aunrecordedpastweek_b_t_r_j <- rbind(aunrecordedpastweek_b_r, aunrecordedpastweek_b_t_r)
aunrecordedpastweek_b_t_r_j

# BOTH SEXES - REGION
aunrecordedpastweek_b_reg <- aunrecordedpastweek_stata_summary(.select_agerange = region, .group_var = region)
aunrecordedpastweek_b_t_reg <- aunrecordedpastweek_stata_summary(.agerange = region, .total = "Total")
aunrecordedpastweek_b_t_reg_j <- rbind(aunrecordedpastweek_b_reg, aunrecordedpastweek_b_t_reg)
aunrecordedpastweek_b_t_reg_j


m_w_b_u_r_reg <- c("m","w","b","m_u","w_u","b_u","m_r","w_r","b_r","b_reg")
aunrecordedpastweek <- 
  list(
    # men, women, both
    aunrecordedpastweek_m_t_j, aunrecordedpastweek_w_t_j, aunrecordedpastweek_b_t_j,
    # urban (men, women, both)
    aunrecordedpastweek_m_t_u_j, aunrecordedpastweek_w_t_u_j, aunrecordedpastweek_b_t_u_j,
    # rural (men, women, both)
    aunrecordedpastweek_m_t_r_j, aunrecordedpastweek_w_t_r_j, aunrecordedpastweek_b_t_r_j,
    # region (both only)
    aunrecordedpastweek_b_t_reg_j) %>% setNames(m_w_b_u_r_reg)

##################################
# WHAT IS PRODUCED WITH SRVYR

# aunrecordedpastweek_list_long <- tbls_mn_summary(.variable = percentunrec * 100)


