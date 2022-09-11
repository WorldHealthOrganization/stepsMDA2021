################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Raised Risk"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "SummaryofCombinedRiskFactors", "functions", "Raisedrisk.R"))

raisedrisk_df <- raisedrisk(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- raisedrisk_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Summary of Combined Risk Factors
raisedrisk_list_long <- tbls_summary(
  .mn_pct_md = pct, .variable = raisedrisk, .cln = rrcln, 
  .agerange_var = agerangerr, .agerange_u_r_var = agerangerr)

# DATABOOK prep
raisedrisk <- tbls_list_split(
  .data = raisedrisk_list_long, .select_var = raisedrisk, .vars_amount_number = 3)

################################################################################

# FACTSHEET

# 36. Percentage with none of the above risk factors
fs_36_raisedrisk_m <- fs_summary(filter(raisedrisk$m, agerangerr == "18–69"), c(3,4,5), Males, .pct)
fs_36_raisedrisk_w <- fs_summary(filter(raisedrisk$w, agerangerr == "18–69"), c(3,4,5), Females, .pct)
fs_36_raisedrisk_b <- fs_summary(filter(raisedrisk$b, agerangerr == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_36_raisedrisk_joint <- cbind(fs_36_raisedrisk_b,
                                fs_36_raisedrisk_m,
                                fs_36_raisedrisk_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with none of the above risk factors", .before = 1)

readr::write_excel_csv(fs_36_raisedrisk_joint, here("FactSheet", "36_fs_raisedrisk.csv"))


# 37. Percentage with three or more of the above risk factors, aged 18 to 44 years
fs_37_raisedrisk_m <- fs_summary(select(raisedrisk$m, 1,9,10,11) %>% filter(agerangerr == "18–44"), c(2,3,4), Males, .pct)
fs_37_raisedrisk_w <- fs_summary(select(raisedrisk$w, 1,9,10,11) %>% filter(agerangerr == "18–44"), c(2,3,4), Females, .pct)
fs_37_raisedrisk_b <- fs_summary(select(raisedrisk$b, 1,9,10,11) %>% filter(agerangerr == "18–44"), c(2,3,4), "Both sexes", .pct)

fs_37_raisedrisk_joint <- cbind(fs_37_raisedrisk_b,
                                fs_37_raisedrisk_m,
                                fs_37_raisedrisk_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with three or more of the above risk factors, aged 18 to 44 years", .before = 1)

readr::write_excel_csv(fs_37_raisedrisk_joint, here("FactSheet", "37_fs_raisedrisk.csv"))


# 38. Percentage with three or more of the above risk factors, aged 45 to 69 years
fs_38_raisedrisk_m <- fs_summary(select(raisedrisk$m, 1,9,10,11) %>% filter(agerangerr == "45–69"), c(2,3,4), Males, .pct)
fs_38_raisedrisk_w <- fs_summary(select(raisedrisk$w, 1,9,10,11) %>% filter(agerangerr == "45–69"), c(2,3,4), Females, .pct)
fs_38_raisedrisk_b <- fs_summary(select(raisedrisk$b, 1,9,10,11) %>% filter(agerangerr == "45–69"), c(2,3,4), "Both sexes", .pct)

fs_38_raisedrisk_joint <- cbind(fs_38_raisedrisk_b,
                                fs_38_raisedrisk_m,
                                fs_38_raisedrisk_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with three or more of the above risk factors, aged 45 to 69 years", .before = 1)

readr::write_excel_csv(fs_38_raisedrisk_joint, here("FactSheet", "38_fs_raisedrisk.csv"))


# 39. Percentage with three or more of the above risk factors, aged 18 to 69 years
fs_39_raisedrisk_m <- fs_summary(filter(raisedrisk$m, agerangerr == "18–69"), c(9,10,11), Males, .pct)
fs_39_raisedrisk_w <- fs_summary(filter(raisedrisk$w, agerangerr == "18–69"), c(9,10,11), Females, .pct)
fs_39_raisedrisk_b <- fs_summary(filter(raisedrisk$b, agerangerr == "18–69"), c(9,10,11), "Both sexes", .pct)

fs_39_raisedrisk_joint <- cbind(fs_39_raisedrisk_b,
                                fs_39_raisedrisk_m,
                                fs_39_raisedrisk_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with three or more of the above risk factors, aged 18 to 69 years", .before = 1)

readr::write_excel_csv(fs_39_raisedrisk_joint, here("FactSheet", "39_fs_raisedrisk.csv"))


################################################################################


