################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Last clinical breast exam"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("BreastCancerScreening", "functions", "Hbreastcancertime.R"))

hbreastcancertime_df <- hbreastcancertime(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hbreastcancertime_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Last clinical breast exam
# The last time when respondents underwent clinical breast examination

# WOMEN
hbreastcancertime_c_w <- pct(
  .data = STEPSClean, .g_var = c(agerange,c), cln == 1, sex == "Women") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))
hbreastcancertime_c_w_t <- pct(
  .data = STEPSClean, .g_var = c, cln == 1, sex == "Women",
  .agerange = agerange, .total = "18–69") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))

hbreastcancertime_c_w_t_j <- rbind(hbreastcancertime_c_w, hbreastcancertime_c_w_t) %>% 
  rename(m1=3, m_low1=4, m_upp1=5, 
         m2=6, m_low2=7, m_upp2=8, 
         m3=9, m_low3=10, m_upp3=11,
         m4=12, m_low4=13, m_upp4=14,
         m5=15, m_low5=16, m_upp5=17)
hbreastcancertime_c_w_t_j


# WOMEN - URBAN
hbreastcancertime_c_w_u <- pct(
  .data = STEPSClean, .g_var = c(agerange2,c), cln == 1, sex == "Women", ur == "Urban") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))
hbreastcancertime_c_w_t_u <- pct(
  .data = STEPSClean, .g_var = c, cln == 1, sex == "Women", ur == "Urban",
  .agerange = agerange2, .total = "18–69") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))

hbreastcancertime_c_w_t_u_j <- rbind(hbreastcancertime_c_w_u, hbreastcancertime_c_w_t_u) %>% 
  rename(m1=3, m_low1=4, m_upp1=5, 
         m2=6, m_low2=7, m_upp2=8, 
         m3=9, m_low3=10, m_upp3=11,
         m4=12, m_low4=13, m_upp4=14,
         m5=15, m_low5=16, m_upp5=17)
hbreastcancertime_c_w_t_u_j


# WOMEN - RURAL
hbreastcancertime_c_w_r <- pct(
  .data = STEPSClean, .g_var = c(agerange2,c), cln == 1, sex == "Women", ur == "Rural") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))
hbreastcancertime_c_w_t_r <- pct(
  .data = STEPSClean, .g_var = c, cln == 1, sex == "Women", ur == "Rural",
  .agerange = agerange2, .total = "18–69") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))

hbreastcancertime_c_w_t_r_j <- rbind(hbreastcancertime_c_w_r, hbreastcancertime_c_w_t_r) %>% 
  rename(m1=3, m_low1=4, m_upp1=5, 
         m2=6, m_low2=7, m_upp2=8, 
         m3=9, m_low3=10, m_upp3=11,
         m4=12, m_low4=13, m_upp4=14,
         m5=15, m_low5=16, m_upp5=17)
hbreastcancertime_c_w_t_r_j


# REGION
hbreastcancertime_c_w_reg <- pct(
  .data = STEPSClean, .g_var = c(region,c), cln == 1, sex == "Women") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))
hbreastcancertime_c_w_t_reg <- pct(
  .data = STEPSClean, .g_var = c, cln == 1, sex == "Women",
  .agerange = region, .total = "Total") %>% 
  pivot_wider(names_from = c, values_from = c(m, m_low, m_upp)) %>% 
  select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17))

hbreastcancertime_c_w_t_reg_j <- rbind(hbreastcancertime_c_w_reg, hbreastcancertime_c_w_t_reg) %>% 
  rename(m1=3, m_low1=4, m_upp1=5, 
         m2=6, m_low2=7, m_upp2=8, 
         m3=9, m_low3=10, m_upp3=11,
         m4=12, m_low4=13, m_upp4=14,
         m5=15, m_low5=16, m_upp5=17)
hbreastcancertime_c_w_t_reg_j

################################################################################






