################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "BMI Classifications"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mbmiclass.R"))

mbmiclass_df <- mbmiclass(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mbmiclass_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# BMI Classifications
mbmiclass_c_list_long <- tbls_pct_summary(.variable = c, .cln = bmicln)

# DATABOOK prep
mbmiclass_c <- tbls_list_split(
  .data = mbmiclass_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# OBESE (BMI ≥ 30)
# MEN
obese30_m <- mbmiclass_c$m %>% select(1,2,12,13,14)

# WOMEN
obese30_w <- mbmiclass_c$w %>% select(1,2,12,13,14)

# BOTH SEXES
obese30_b <- mbmiclass_c$b %>% select(1,2,12,13,14)

################################################################################

# OVERWEIGHT (BMI ≥ 25)
mbmiclass_d_list_long <- tbls_pct_summary(.variable = d, .cln = bmicln)

# DATABOOK prep
mbmiclass_d <- tbls_list_split(
  .data = mbmiclass_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "3) Overweight >=25")

################################################################################

# FACTSHEET

# 22. Percentage who are overweight (BMI ≥ 25 kg/m2)
fs_22_mbmiclass_d_m <- fs_summary(filter(mbmiclass_d$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_22_mbmiclass_d_w <- fs_summary(filter(mbmiclass_d$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_22_mbmiclass_d_b <- fs_summary(filter(mbmiclass_d$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_22_mbmiclass_d_joint <- cbind(fs_22_mbmiclass_d_b,
                                 fs_22_mbmiclass_d_m,
                                 fs_22_mbmiclass_d_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who are overweight (BMI ≥ 25 kg/m2)", .before = 1)
fs_22_mbmiclass_d_joint

readr::write_excel_csv(fs_22_mbmiclass_d_joint, here("FactSheet", "22_fs_mbmiclass_d.csv"))

# 23. Percentage who are obese (BMI ≥ 30 kg/m2)
fs_23_obese30_m <- fs_summary(filter(obese30_m, agerange=="18–69"), c(3,4,5), Males, .pct)
fs_23_obese30_w <- fs_summary(filter(obese30_w, agerange=="18–69"), c(3,4,5), Females, .pct)
fs_23_obese30_b <- fs_summary(filter(obese30_b, agerange=="18–69"), c(3,4,5), "Both sexes", .pct)

fs_23_obese30_joint <- cbind(fs_23_obese30_b,
                             fs_23_obese30_m,
                             fs_23_obese30_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who are obese (BMI ≥ 30 kg/m2)", .before = 1)
fs_23_obese30_joint

readr::write_excel_csv(fs_23_obese30_joint, here("FactSheet", "23_fs_obese30.csv"))


################################################################################


