################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Average Waist Circumference"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mwaist.R"))

mwaist_df <- mwaist(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mwaist_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

mwaist_list_long <- tbls_summary(.mn_pct_md = mn, .variable = m14, .cln = m14cln)

# DATABOOK prep
mwaist <- tbls_list_split(.data = mwaist_list_long, .vars_amount_number = 0)

################################################################################

# FACTSHEET

# 24. Average waist circumference (cm)
fs_24_mwaist_m <- fs_summary(filter(mwaist$m, agerange == "18–69"), c(3,4,5), Males)
fs_24_mwaist_w <- fs_summary(filter(mwaist$w, agerange == "18–69"), c(3,4,5), Females)

fs_24_mwaist_joint <- cbind(fs_24_mwaist_m,
                            fs_24_mwaist_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Average waist circumference (cm)", .before = 1)

readr::write_excel_csv(fs_24_mwaist_joint, here("FactSheet", "24_fs_mwaist.csv"))

################################################################################

