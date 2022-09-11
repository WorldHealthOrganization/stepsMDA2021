################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "CVD Risk: EUR B"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "CardiovascularDiseaseRisk", "functions", "CVDrisk.R"))

cvdrisk_df <- cvdrisk(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cvdrisk_df %>%
  as_survey_design(ids=psu, strata=stratum, weights=wstep3, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Percentage of respondents with a 10-year CVD risk ≥30% or with existing CVD
cvdrisk_c_list_long <- tbls_summary(
  .mn_pct_md = pct, .variable = c, .agerange_var = agerangecvd, .agerange_u_r_var = agerangecvd)

# DATABOOK prep
cvdrisk_c <- tbls_list_split(
  .data = cvdrisk_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "risk 30% or more or has CVD")

################################################################################

# 2 - Percentage of eligible persons receiving drug therapy and counseling to prevent heart attacks and strokes
cvdrisk_d_list_long <- tbls_summary(
  .mn_pct_md = pct, .variable = d, .cln2 = highrisk, .cln2_val = 1, 
  .agerange_var = agerangecvd, .agerange_u_r_var = agerangecvd)

# DATABOOK prep
cvdrisk_d <- tbls_list_split(
  .data = cvdrisk_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) received drug therapy and counseling")

################################################################################
################################################################################

# FACTSHEET

# 35. Percentage aged 40–69 years with a 10-year CVD risk ≥ 30%, or with existing CVD**
# 
# ** A 10-year CVD risk of ≥30% is defined according to age, sex, blood pressure, 
# smoking status (current smokers OR those who quit smoking less than 1 year 
# before the assessment), total cholesterol, and diabetes (previously diagnosed 
# OR a fasting plasma glucose concentration >7.0 mmol/l (126 mg/dl).
# 
# Summary of combined risk factors
# •	current daily smokers
# •	less than 5 servings of fruits & vegetables per day
# •	insufficient physical activity	
# •	overweight (BMI ≥ 25 kg/m2)
# •	raised BP (SBP ≥ 140 and/or DBP ≥ 90 mmHg or currently on medication for raised BP)

fs_35_cvdrisk_c_m <- fs_summary(filter(cvdrisk_c$m, agerangecvd == "18–69"), c(3,4,5), Males, .pct)
fs_35_cvdrisk_c_w <- fs_summary(filter(cvdrisk_c$w, agerangecvd == "18–69"), c(3,4,5), Females, .pct)
fs_35_cvdrisk_c_b <- fs_summary(filter(cvdrisk_c$b, agerangecvd == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_35_cvdrisk_c_joint <- cbind(fs_35_cvdrisk_c_b,
                               fs_35_cvdrisk_c_m,
                               fs_35_cvdrisk_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage aged 40–69 years with a 10-year CVD risk ≥ 30%, or with existing CVD**", .before = 1)

readr::write_excel_csv(fs_35_cvdrisk_c_joint, here("FactSheet", "35_fs_cvdrisk_c.csv"))

################################################################################

