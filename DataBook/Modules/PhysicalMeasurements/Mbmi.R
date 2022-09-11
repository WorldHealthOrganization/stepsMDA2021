################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Mean BMI"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mbmi.R"))

mbmi_df <- mbmi(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mbmi_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Mean height (cm)
# ONLY MEN & WOMEN are included in data book
mbmi_m11_list_long <- tbls_summary(.mn_pct_md = mn, .variable = m11, .cln = m11cln)

# DATABOOK prep
mbmi_m11 <- 
  tbls_list_split(.data = mbmi_m11_list_long, .vars_amount_number = 0)

################################################################################

# 2 - Mean weight (kg)
# ONLY MEN & WOMEN are included in data book
mbmi_m12_list_long <- tbls_summary(.mn_pct_md = mn, .variable = m12, .cln = m12cln)

# DATABOOK prep
mbmi_m12 <- 
  tbls_list_split(.data = mbmi_m12_list_long, .vars_amount_number = 0)

################################################################################

# 3 - Mean BMI (kg/m2)
mbmi_list_long <- tbls_summary(.mn_pct_md = mn, .variable = bmi, .cln = bmicln)

# DATABOOK prep
mbmi <- tbls_list_split(.data = mbmi_list_long, .vars_amount_number = 0)

################################################################################
################################################################################

# FACTSHEET

# 21. Mean body mass index - BMI (kg/m2)
fs_21_mbmi_m <- fs_summary(filter(mbmi$m, agerange == "18–69"), c(3,4,5), Males)
fs_21_mbmi_w <- fs_summary(filter(mbmi$w, agerange == "18–69"), c(3,4,5), Females)
fs_21_mbmi_b <- fs_summary(filter(mbmi$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_21_mbmi_joint <- cbind(fs_21_mbmi_b,
                          fs_21_mbmi_m,
                          fs_21_mbmi_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean body mass index – BMI (kg/m2)", .before = 1)

readr::write_excel_csv(fs_21_mbmi_joint, here("FactSheet", "21_fs_mbmi.csv"))

################################################################################




