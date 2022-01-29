################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Blood Pressure Status, Treatment and Control"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mraisedbp.R"))

mraisedbp_df <- mraisedbp(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mraisedbp_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - RAISED BP
# 1.1 - SBP ≥140 and/or DBP ≥ 90 mmHg
raisedbp_140_90_list_long <- tbls_pct_summary(.variable = raisedbp_140_90)
# DATABOOK prep
raisedbp_140_90 <- tbls_list_split(
  .data = raisedbp_140_90_list_long, .select_var = raisedbp_140_90, 
  .vars_amount_number = 2, .select_var_val = "1)SBP>=140 and/or DBP>=90")

# 1.2 - SBP ≥160 and/or DBP ≥ 100 mmHg
raisedbp_160_100_list_long <- tbls_pct_summary(.variable = raisedbp_160_100)
# DATABOOK prep
raisedbp_160_100 <- tbls_list_split(
  .data = raisedbp_160_100_list_long, .select_var = raisedbp_160_100, 
  .vars_amount_number = 2, .select_var_val = "1)SBP>=160 and/or DBP>=100")

################################################################################
# 2 - Raised BP, excluding those on medication # NOT INCLUDED IN DATA BOOK
# raisedbp_140_90_excl_ppl_on_meds_list_long <- tbls_pct_summary(.variable = raisedbp_140_90, .cln = clnnomeds)
# # DATABOOK prep
# raisedbp_140_90_excl_ppl_on_meds <- tbls_list_split(
#   .data = raisedbp_140_90_excl_ppl_on_meds_list_long, .select_var = raisedbp_140_90, 
#   .vars_amount_number = 2, .select_var_val = "1)SBP>=140 and/or DBP>=90")

################################################################################

# 3 - Raised BP or on medication
# 3.1 - SBP ≥140 and/or DBP ≥ 90 mmHg or currently on medication for raised blood pressure
raisedbp_140_90_or_meds_list_long <- tbls_pct_summary(.variable = raisedbp_140_90_or_meds)
# DATABOOK prep
raisedbp_140_90_or_meds <- tbls_list_split(
  .data = raisedbp_140_90_or_meds_list_long, .select_var = raisedbp_140_90_or_meds, 
  .vars_amount_number = 2, .select_var_val = "1)SBP>=140 and/or DBP>=90 or on meds")


# 3.2 - SBP ≥160 and/or DBP ≥ 100 mmHg or currently on medication for raised blood pressure
raisedbp_160_100_or_meds_list_long <- tbls_pct_summary(.variable = raisedbp_160_100_or_meds)
# DATABOOK prep
raisedbp_160_100_or_meds <- tbls_list_split(
  .data = raisedbp_160_100_or_meds_list_long, .select_var = raisedbp_160_100_or_meds, 
  .vars_amount_number = 2, .select_var_val = "1)SBP>=160 and/or DBP>=100 or on meds")

################################################################################

# 4 - Hypertension control
# Raised blood pressure diagnosis, treatment and control among those with raised 
# blood pressure (SBP ≥ 140 and/or DBP ≥ 90 mmHg) or on medication for raised blood pressure
# NOTE: new/current version of tables split by 4 columns
htn_control_list_long <- tbls_pct_summary(.variable = htn_control, .cln = htn_control_cln)

# DATABOOK prep
htn_control <- tbls_list_split(
  .data = htn_control_list_long, .select_var = htn_control, .vars_amount_number = 4)


# 4.1 - Hypertension control / OLD version (presented as in the previous STEPS round in 2016) 
# It is also used on FactSheet (indicator #28)
# Raised blood pressure diagnosis, treatment and control among those with raised 
# blood pressure (SBP ≥ 140 and/or DBP ≥ 90 mmHg) or on medication for raised blood pressure
# NOTE: this is how tables were presented previously - split by 3 columns instead of 4
# The last column from old ("% not on medication and BP raised") is now split by two and positioned 
# at the beginning of the table (new column names: 
# "% with raised blood pressure, not previously diagnosed", 
# "% with previously diagnosed raised blood pressure, not on medication")
bp_control_old_list_long <- tbls_pct_summary(.variable = bp_control_old, .cln = htn_control_cln)

# DATABOOK prep
bp_control_old <- tbls_list_split(
  .data = bp_control_old_list_long, .select_var = bp_control_old, .vars_amount_number = 3)


################################################################################

# FACTSHEET

# 27. Percentage with raised BP (SBP ≥ 140 and/or DBP ≥ 90 mmHg or currently on medication for raised BP)
fs_27_raisedbp_140_90_or_meds_m <- fs_summary(filter(raisedbp_140_90_or_meds$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_27_raisedbp_140_90_or_meds_w <- fs_summary(filter(raisedbp_140_90_or_meds$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_27_raisedbp_140_90_or_meds_b <- fs_summary(filter(raisedbp_140_90_or_meds$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_27_raisedbp_140_90_or_meds_joint <- cbind(fs_27_raisedbp_140_90_or_meds_b,
                                             fs_27_raisedbp_140_90_or_meds_m,
                                             fs_27_raisedbp_140_90_or_meds_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with raised BP (SBP ≥ 140 and/or DBP ≥ 90 mmHg or currently on medication for raised BP)", .before = 1)
fs_27_raisedbp_140_90_or_meds_joint

readr::write_excel_csv(fs_27_raisedbp_140_90_or_meds_joint, here("FactSheet", "27_fs_raisedbp_140_90_or_meds.csv"))


# 28. Percentage with raised BP (SBP ≥ 140 and/or DBP ≥ 90 mmHg or currently on
# medication for raised BP) who are not currently on medication for raised BP
fs_28_bp_control_old_m <- fs_summary(filter(bp_control_old$m, agerange == "18–69"), c(9,10,11), Males, .pct)
fs_28_bp_control_old_w <- fs_summary(filter(bp_control_old$w, agerange == "18–69"), c(9,10,11), Females, .pct)
fs_28_bp_control_old_b <- fs_summary(filter(bp_control_old$b, agerange == "18–69"), c(9,10,11), "Both sexes", .pct)

fs_28_bp_control_old_joint <- cbind(fs_28_bp_control_old_b,
                                    fs_28_bp_control_old_m,
                                    fs_28_bp_control_old_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with raised BP (SBP ≥ 140 and/or DBP ≥ 90 mmHg or currently on medication for raised BP) who are not currently on medication for raised BP", .before = 1)
fs_28_bp_control_old_joint

readr::write_excel_csv(fs_28_bp_control_old_joint, here("FactSheet", "28_fs_bp_control_old.csv"))


################################################################################


