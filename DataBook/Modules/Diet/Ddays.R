################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Mean number of days fruit or vegetables consumed"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Ddays.R"))

ddays_df <- ddays(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ddays_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of days fruit consumed in a typical week
ddays_d1_list_long <- tbls_summary(.mn_pct_md = mn, .variable = d1, .cln = d1cln)

# DATABOOK prep
ddays_d1 <- 
  tbls_list_split(.data = ddays_d1_list_long, .vars_amount_number = 0)

################################################################################

# Mean number of days vegetables consumed in a typical week
ddays_d3_list_long <- tbls_summary(.mn_pct_md = mn, .variable = d3, .cln = d3cln)

# DATABOOK prep
ddays_d3 <- 
  tbls_list_split(.data = ddays_d3_list_long, .vars_amount_number = 0)

################################################################################
################################################################################

# FACTSHEET

# 10. Mean number of days fruit consumed in a typical week
fs_10_ddays_d1_m <- fs_summary(filter(ddays_d1$m, agerange == "18–69"), c(3,4,5), Males)
fs_10_ddays_d1_w <- fs_summary(filter(ddays_d1$w, agerange == "18–69"), c(3,4,5), Females)
fs_10_ddays_d1_b <- fs_summary(filter(ddays_d1$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_10_ddays_d1_joint <- cbind(fs_10_ddays_d1_b,
                              fs_10_ddays_d1_m,
                              fs_10_ddays_d1_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean number of days fruit consumed in a typical week", .before = 1)

readr::write_excel_csv(fs_10_ddays_d1_joint, here("FactSheet", "10_fs_ddays_d1.csv"))


# 12. Mean number of days vegetables consumed in a typical week
fs_12_ddays_d3_m <- fs_summary(filter(ddays_d3$m, agerange == "18–69"), c(3,4,5), Males)
fs_12_ddays_d3_w <- fs_summary(filter(ddays_d3$w, agerange == "18–69"), c(3,4,5), Females)
fs_12_ddays_d3_b <- fs_summary(filter(ddays_d3$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_12_ddays_d3_joint <- cbind(fs_12_ddays_d3_b,
                              fs_12_ddays_d3_m,
                              fs_12_ddays_d3_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean number of days vegetables consumed in a typical week", .before = 1)

readr::write_excel_csv(fs_12_ddays_d3_joint, here("FactSheet", "12_fs_ddays_d3.csv"))

################################################################################
################################################################################


