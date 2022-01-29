################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Mean Systolic and Diastolic Blood Pressure"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mbloodpressure.R"))

mbloodpressure_df <- mbloodpressure(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mbloodpressure_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Including those on meds: Mean SBP
# Mean systolic blood pressure (mmHg)
mbloodpressure_sbp_list_long <- tbls_mn_summary(.variable = sbp, .cln = sbpcln)

# DATABOOK prep
mbloodpressure_sbp <- 
  tbls_list_split(.data = mbloodpressure_sbp_list_long, .vars_amount_number = 0)

################################################################################

# Including those on meds: Mean DBP
# Mean diastolic blood pressure (mmHg)
mbloodpressure_dbp_list_long <- tbls_mn_summary(.variable = dbp, .cln = dbpcln)

# DATABOOK prep
mbloodpressure_dbp <- 
  tbls_list_split(.data = mbloodpressure_dbp_list_long, .vars_amount_number = 0)

################################################################################
################################################################################

# FACTSHEET

# # 25. Mean systolic blood pressure - SBP (mmHg), including those currently on medication for raised BP
# fs_25_mbloodpressure_sbp_m <- fs_summary(filter(mbloodpressure_sbp$m, agerange == "18–69"), c(3,4,5), Males)
# fs_25_mbloodpressure_sbp_w <- fs_summary(filter(mbloodpressure_sbp$w, agerange == "18–69"), c(3,4,5), Females)
# fs_25_mbloodpressure_sbp_b <- fs_summary(filter(mbloodpressure_sbp$b, agerange == "18–69"), c(3,4,5), "Both sexes")
# 
# fs_25_mbloodpressure_sbp_joint <- cbind(fs_25_mbloodpressure_sbp_b,
#                                         fs_25_mbloodpressure_sbp_m, 
#                                         fs_25_mbloodpressure_sbp_w) %>% 
#   mutate("Results for adults aged 18–69 years (incl. 95% CI)" = 
#            "Mean systolic blood pressure – SBP (mmHg), including those currently on medication for raised BP", .before = 1)
# fs_25_mbloodpressure_sbp_joint
# 
# readr::write_excel_csv(fs_25_mbloodpressure_sbp_joint, here("FactSheet", "25_fs_mbloodpressure_sbp.csv"))
# 
# 
# # 26. Mean diastolic blood pressure - DBP (mmHg), including those currently on medication for raised BP
# fs_26_mbloodpressure_dbp_m <- fs_summary(filter(mbloodpressure_dbp$m, agerange == "18–69"), c(3,4,5), Males)
# fs_26_mbloodpressure_dbp_w <- fs_summary(filter(mbloodpressure_dbp$w, agerange == "18–69"), c(3,4,5), Females)
# fs_26_mbloodpressure_dbp_b <- fs_summary(filter(mbloodpressure_dbp$b, agerange == "18–69"), c(3,4,5), "Both sexes")
# 
# fs_26_mbloodpressure_dbp_joint <- cbind(fs_26_mbloodpressure_dbp_b,
#                                         fs_26_mbloodpressure_dbp_m, 
#                                         fs_26_mbloodpressure_dbp_w) %>% 
#   mutate("Results for adults aged 18–69 years (incl. 95% CI)" = 
#            "Mean diastolic blood pressure – DBP (mmHg), including those currently on medication for raised BP", .before = 1)
# fs_26_mbloodpressure_dbp_joint
# 
# readr::write_excel_csv(fs_26_mbloodpressure_dbp_joint, here("FactSheet", "26_fs_mbloodpressure_dbp.csv"))


################################################################################
