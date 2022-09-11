################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Manufactured cigarette smokers among daily and current smokers"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokeman.R"))

tsmokeman_df <- tsmokeman(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokeman_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# ***DAILY SMOKERS
# Manufactured cigarette smokers among daily smokers

tsmokeman_daily_list_long <- 
  tbls_summary(.mn_pct_md = pct, .variable = c, .cln2 = t2, .cln2_val = 1)

# DATABOOK prep
tsmokeman_daily <- tbls_list_split(
  .data = tsmokeman_daily_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "smokes manufactured cig")

################################################################################

# ***NONDAILY SMOKERS
# Manufactured cigarette smokers among current smokers

tsmokeman_nondaily_list_long <- 
  tbls_summary(.mn_pct_md = pct, .variable = c, .cln2 = t1, .cln2_val = 1)

# DATABOOK prep
tsmokeman_nondaily <- tbls_list_split(
  .data = tsmokeman_nondaily_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "smokes manufactured cig")

################################################################################
################################################################################

# FACTSHEET

# 4. Percentage of daily smokers smoking manufactured cigarettes
fs_tsmokeman_daily_m <- fs_summary(filter(tsmokeman_daily$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_tsmokeman_daily_w <- fs_summary(filter(tsmokeman_daily$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_tsmokeman_daily_b <- fs_summary(filter(tsmokeman_daily$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_tsmokeman_daily_joint <- cbind(fs_tsmokeman_daily_b, fs_tsmokeman_daily_m, fs_tsmokeman_daily_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage of daily smokers smoking manufactured cigarettes", .before = 1)

readr::write_excel_csv(fs_tsmokeman_daily_joint, here("FactSheet", "04_fs_tsmokeman_daily.csv"))

################################################################################
################################################################################


