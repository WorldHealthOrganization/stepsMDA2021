################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Breast cancer screening"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("BreastCancerScreening", "functions", "Hbreastcancer.R"))

hbreastcancer_df <- hbreastcancer(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hbreastcancer_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Breast cancer screening
# Percentage of respondents that underwent breast cancer screening tests

hbreastcancer_c_w <- pct(.data = STEPSClean, .g_var = c(agerange, c), .fltr_var = c, 
                         .fltr_val = "1) has been screened", cln == 1, sex == "Women")
hbreastcancer_c_w_t <- pct(.data = STEPSClean, .g_var = c, .fltr_var = c, 
                           .fltr_val = "1) has been screened", cln == 1, sex == "Women",
                           .agerange = agerange, .total = "18–69")
hbreastcancer_c_w_t_j <- rbind(hbreastcancer_c_w, hbreastcancer_c_w_t) %>% select(-c)
hbreastcancer_c_w_t_j

# URBAN
hbreastcancer_c_w_u <- pct(.data = STEPSClean, .g_var = c(agerange2, c), .fltr_var = c, 
                           .fltr_val = "1) has been screened", cln == 1, sex == "Women", ur == "Urban")
hbreastcancer_c_w_t_u <- pct(.data = STEPSClean, .g_var = c, .fltr_var = c, 
                             .fltr_val = "1) has been screened", cln == 1, sex == "Women", ur == "Urban",
                             .agerange = agerange2, .total = "18–69")
hbreastcancer_c_w_t_u_j <- rbind(hbreastcancer_c_w_u, hbreastcancer_c_w_t_u) %>% select(-c)
hbreastcancer_c_w_t_u_j

# RURAL
hbreastcancer_c_w_r <- pct(.data = STEPSClean, .g_var = c(agerange2, c), .fltr_var = c, 
                           .fltr_val = "1) has been screened", cln == 1, sex == "Women", ur == "Rural")
hbreastcancer_c_w_t_r <- pct(.data = STEPSClean, .g_var = c, .fltr_var = c, 
                             .fltr_val = "1) has been screened", cln == 1, sex == "Women", ur == "Rural",
                             .agerange = agerange2, .total = "18–69")
hbreastcancer_c_w_t_r_j <- rbind(hbreastcancer_c_w_r, hbreastcancer_c_w_t_r) %>% select(-c)
hbreastcancer_c_w_t_r_j

# REGION
hbreastcancer_c_w_reg <- pct(.data = STEPSClean, .g_var = c(region, c), .fltr_var = c, 
                             .fltr_val = "1) has been screened", cln == 1, sex == "Women")
hbreastcancer_c_w_t_reg <- pct(.data = STEPSClean, .g_var = c, .fltr_var = c, 
                               .fltr_val = "1) has been screened", cln == 1, sex == "Women",
                               .agerange = region, .total = "Total")
hbreastcancer_c_w_t_reg_j <- rbind(hbreastcancer_c_w_reg, hbreastcancer_c_w_t_reg) %>% select(-c)
hbreastcancer_c_w_t_reg_j


################################################################################






