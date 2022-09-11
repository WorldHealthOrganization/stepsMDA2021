################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Raised total cholesterol and mean total cholesterol"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "BiochemicalMeasurements", "functions", "Btotallipids.R"))

btotallipids_df <- btotallipids(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- btotallipids_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep3, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean total cholesterol 
# 1.1 - (mmol/L), including those on meds
btotallipids_b8_list_long <- tbls_summary(.mn_pct_md = mn, .variable = b8)

# DATABOOK prep
btotallipids_b8 <- 
  tbls_list_split(.data = btotallipids_b8_list_long, .vars_amount_number = 0)

################################################################################

# 1.2 - (mg/dl), including those on meds
btotallipids_b8mg_list_long <- tbls_summary(.mn_pct_md = mn, .variable = b8mg)

# DATABOOK prep
btotallipids_b8mg <- 
  tbls_list_split(.data = btotallipids_b8mg_list_long, .vars_amount_number = 0)

################################################################################

# 2.1 - Total cholesterol ≥ 5.0 mmol/L or ≥ 190 mg/dl
btotallipids_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e)

# DATABOOK prep
btotallipids_e <- tbls_list_split(
  .data = btotallipids_e_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "total cholesterol >= 5.0")

################################################################################

# 2.2 - Total cholesterol ≥ 6.2 mmol/L or ≥ 240 mg/dl
# NOTE: PRODUCES SLIGHTLY DIFFERENT NUMBERS
btotallipids_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f)

# DATABOOK prep
btotallipids_f <- tbls_list_split(
  .data = btotallipids_f_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "total cholesterol >= 6.2")

################################################################################

# 3.1 - Total cholesterol ≥ 5.0 mmol/L or ≥ 190 mg/dl or currently on medication for raised cholesterol
btotallipids_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
btotallipids_c <- tbls_list_split(
  .data = btotallipids_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "total cholesterol >= 5.0 or on meds")

################################################################################

# 3.2 - Total cholesterol ≥ 6.2 mmol/L or ≥ 240 mg/dl or currently on medication for raised cholesterol
# NOTE: PRODUCES SLIGHTLY DIFFERENT NUMBERS
btotallipids_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
btotallipids_d <- tbls_list_split(
  .data = btotallipids_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "total cholesterol >= 6.2 or on meds")

################################################################################

# FACTSHEET

# 32. Mean total blood cholesterol, including those currently on medication for raised cholesterol (mmol/L)
fs_32_btotallipids_b8_m <- fs_summary(filter(btotallipids_b8$m, agerange == "18–69"), c(3,4,5), Males)
fs_32_btotallipids_b8_w <- fs_summary(filter(btotallipids_b8$w, agerange == "18–69"), c(3,4,5), Females)
fs_32_btotallipids_b8_b <- fs_summary(filter(btotallipids_b8$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_32_btotallipids_b8_joint <- cbind(fs_32_btotallipids_b8_b,
                                     fs_32_btotallipids_b8_m,
                                     fs_32_btotallipids_b8_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean total blood cholesterol, including those currently on medication for raised cholesterol (mmol/L)", .before = 1)

readr::write_excel_csv(fs_32_btotallipids_b8_joint, here("FactSheet", "32_fs_btotallipids_b8.csv"))


# 33. Percentage with raised total cholesterol (≥ 5.0 mmol/L or ≥ 190 mg/dl or currently on medication for raised cholesterol)
fs_33_btotallipids_c_m <- fs_summary(filter(btotallipids_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_33_btotallipids_c_w <- fs_summary(filter(btotallipids_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_33_btotallipids_c_b <- fs_summary(filter(btotallipids_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_33_btotallipids_c_joint <- cbind(fs_33_btotallipids_c_b,
                                    fs_33_btotallipids_c_m,
                                    fs_33_btotallipids_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with raised total cholesterol (≥ 5.0 mmol/L or ≥ 190 mg/dl or currently on medication for raised cholesterol)", .before = 1)

readr::write_excel_csv(fs_33_btotallipids_c_joint, here("FactSheet", "33_fs_btotallipids_c.csv"))

################################################################################



