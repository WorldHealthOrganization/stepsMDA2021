################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Blood glucose status and mean glucose"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "BiochemicalMeasurements", "functions", "Bglucose.R"))

bglucose_df <- bglucose(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- bglucose_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep3, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - (mmol/L), including those on meds - Mean fasting blood glucose (mmol/L)
bglucose_b5_list_long <- tbls_mn_summary(.variable = b5)

# DATABOOK prep
bglucose_b5 <- tbls_list_split(.data = bglucose_b5_list_long, .vars_amount_number = 0)

################################################################################

# 2 - (mg/dl), including those on meds - Mean fasting blood glucose (mg/dl)
bglucose_b5mg_list_long <- tbls_mn_summary(.variable = b5mg)

# DATABOOK prep
bglucose_b5mg <- tbls_list_split(.data = bglucose_b5mg_list_long, .vars_amount_number = 0)

################################################################################

# 3 - for portable devices reporting plasma-equivalent values or for plasma venous blood 
# collected with wet chemistry method (contact the STEPS team if neither applies to your data)

# 3.1 - Impaired Fasting Glycaemia
bglucose_ifg_d_list_long <- tbls_pct_summary(.variable = d, .cln = clnall)

# DATABOOK prep
bglucose_ifg_d <- tbls_list_split(
  .data = bglucose_ifg_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "2) blood glucose >=6.1 AND <7.0")

################################################################################

# 3.2 - Raised blood glucose or currently on medication for diabetes
bglucose_rbg_or_meds_d_list_long <- tbls_pct_summary(.variable = d, .cln = clnall)

# DATABOOK prep
bglucose_rbg_or_meds_d <- tbls_list_split(
  .data = bglucose_rbg_or_meds_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "3) blood glucose >=7.0 or took meds today")

################################################################################

# 4 - all respondents - Currently on medication for diabetes
bglucose_medstext_list_long <- tbls_pct_summary(.variable = medstext, .cln = FALSE, .cln_val = FALSE)

# DATABOOK prep
bglucose_medstext <- tbls_list_split(
  .data = bglucose_medstext_list_long, 
  .select_var = medstext, .vars_amount_number = 2,
  .select_var_val = "1) currently on treatment for raised blood glucose")

################################################################################

# 5 - Raised blood glucose diagnosis and treatment among all respondents
# NOTE: you need to prepare data for the following tables in #5 by filtering NA values in diagn
bglucose_diagn_list_long <- 
  tbls_pct_summary(.data = STEPSClean %>% filter(!is.na(diagn)), .variable = diagn)

# DATABOOK prep
bglucose_diagn <- 
  tbls_list_split(.data = bglucose_diagn_list_long, .select_var = diagn, .vars_amount_number = 4) %>% 
  map(~ select(., -c(m4,m_low4,m_upp4))) # drop unused columns

################################################################################

# FACTSHEET

# 29. Mean fasting blood glucose, including those currently on medication for raised blood glucose (mmol/L)
fs_29_bglucose_b5_m <- fs_summary(filter(bglucose_b5$m, agerange == "18–69"), c(3,4,5), Males)
fs_29_bglucose_b5_w <- fs_summary(filter(bglucose_b5$w, agerange == "18–69"), c(3,4,5), Females)
fs_29_bglucose_b5_b <- fs_summary(filter(bglucose_b5$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_29_bglucose_b5_joint <- cbind(fs_29_bglucose_b5_b,
                                 fs_29_bglucose_b5_m,
                                 fs_29_bglucose_b5_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean fasting blood glucose, including those currently on medication for raised blood glucose (mmol/L)", .before = 1)
fs_29_bglucose_b5_joint

readr::write_excel_csv(fs_29_bglucose_b5_joint, here("FactSheet", "29_fs_bglucose_b5.csv"))


# 30. Percentage with impaired fasting glycaemia as defined below
# •	capillary whole blood value ≥5.6 mmol/L (100 mg/dl) and <6.1 mmol/L (110 mg/dl)
fs_30_bglucose_ifg_d_m <- fs_summary(filter(bglucose_ifg_d$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_30_bglucose_ifg_d_w <- fs_summary(filter(bglucose_ifg_d$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_30_bglucose_ifg_d_b <- fs_summary(filter(bglucose_ifg_d$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_30_bglucose_ifg_d_joint <- cbind(fs_30_bglucose_ifg_d_b,
                                    fs_30_bglucose_ifg_d_m,
                                    fs_30_bglucose_ifg_d_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with impaired fasting glycaemia as defined below
         •	capillary whole blood value ≥5.6 mmol/L (100 mg/dl) and <6.1 mmol/L (110 mg/dl)", .before = 1)
fs_30_bglucose_ifg_d_joint

readr::write_excel_csv(fs_30_bglucose_ifg_d_joint, here("FactSheet", "30_fs_bglucose_ifg_d.csv"))


# 31. Percentage with raised fasting blood glucose as defined below or currently on medication for raised blood glucose
# •	capillary whole blood value ≥ 6.1 mmol/L (110 mg/dl)
fs_31_bglucose_rbg_or_meds_d_m <- fs_summary(filter(bglucose_rbg_or_meds_d$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_31_bglucose_rbg_or_meds_d_w <- fs_summary(filter(bglucose_rbg_or_meds_d$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_31_bglucose_rbg_or_meds_d_b <- fs_summary(filter(bglucose_rbg_or_meds_d$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_31_bglucose_rbg_or_meds_d_joint <- cbind(fs_31_bglucose_rbg_or_meds_d_b,
                                            fs_31_bglucose_rbg_or_meds_d_m,
                                            fs_31_bglucose_rbg_or_meds_d_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with raised fasting blood glucose as defined below or currently on medication for raised blood glucose
           •  capillary whole blood value ≥ 6.1 mmol/L (110 mg/dl)", .before = 1)
fs_31_bglucose_rbg_or_meds_d_joint

readr::write_excel_csv(fs_31_bglucose_rbg_or_meds_d_joint, here("FactSheet", "31_fs_bglucose_rbg_or_meds_d.csv"))

################################################################################

