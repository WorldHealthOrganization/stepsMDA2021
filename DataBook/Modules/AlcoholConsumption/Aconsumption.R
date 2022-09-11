################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Drinking status of total population"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aconsumption.R"))

aconsumption_df <- aconsumption(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aconsumption_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Alcohol consumption status
aconsumption_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
aconsumption_c <- 
  tbls_list_split(.data = aconsumption_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################
################################################################################

# FACTSHEET

# 6. Percentage who are lifetime abstainers
fs_6_aconsumption_c_m <- fs_summary(filter(aconsumption_c$m, agerange == "18–69"), c(12,13,14), Males, .pct)
fs_6_aconsumption_c_w <- fs_summary(filter(aconsumption_c$w, agerange == "18–69"), c(12,13,14), Females, .pct)
fs_6_aconsumption_c_b <- fs_summary(filter(aconsumption_c$b, agerange == "18–69"), c(12,13,14), "Both sexes", .pct)

fs_6_aconsumption_c_joint <- cbind(fs_6_aconsumption_c_b,
                                   fs_6_aconsumption_c_m,
                                   fs_6_aconsumption_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who are lifetime abstainers", .before = 1)

readr::write_excel_csv(fs_6_aconsumption_c_joint, here("FactSheet", "06_fs_aconsumption_c.csv"))

# 7. Percentage who are past 12-month abstainers
fs_7_aconsumption_c_m <- fs_summary(filter(aconsumption_c$m, agerange == "18–69"), c(9,10,11), Males, .pct)
fs_7_aconsumption_c_w <- fs_summary(filter(aconsumption_c$w, agerange == "18–69"), c(9,10,11), Females, .pct)
fs_7_aconsumption_c_b <- fs_summary(filter(aconsumption_c$b, agerange == "18–69"), c(9,10,11), "Both sexes", .pct)

fs_7_aconsumption_c_joint <- cbind(fs_7_aconsumption_c_b,
                                   fs_7_aconsumption_c_m,
                                   fs_7_aconsumption_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who are past 12-month abstainers", .before = 1)

readr::write_excel_csv(fs_7_aconsumption_c_joint, here("FactSheet", "07_fs_aconsumption_c.csv"))


# 8. Percentage who currently drink (drank alcohol in the past 30 days)
fs_8_aconsumption_c_m <- fs_summary(filter(aconsumption_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_8_aconsumption_c_w <- fs_summary(filter(aconsumption_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_8_aconsumption_c_b <- fs_summary(filter(aconsumption_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_8_aconsumption_c_joint <- cbind(fs_8_aconsumption_c_b,
                                   fs_8_aconsumption_c_m,
                                   fs_8_aconsumption_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who currently drink (drank alcohol in the past 30 days)", .before = 1)

readr::write_excel_csv(fs_8_aconsumption_c_joint, here("FactSheet", "08_fs_aconsumption_c.csv"))

################################################################################
################################################################################

