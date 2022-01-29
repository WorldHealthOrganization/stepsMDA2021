################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Mean number of servings of fruit and/or vegetables consumed per day"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dservings.R"))

dservings_df <- dservings(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dservings_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1
# Mean number of servings of fruit on average per day
fservings_list_long <- tbls_mn_summary(.variable = fservings, .cln = fruitcln)

# DATABOOK prep
fservings_tbls <- tbls_list_split(.data = fservings_list_long, .vars_amount_number = 0)

################################################################################

# 2
# Mean number of servings of vegetables on average per day
vservings_list_long <- tbls_mn_summary(.variable = vservings, .cln = vegcln)

# DATABOOK prep
vservings_tbls <- tbls_list_split(.data = vservings_list_long, .vars_amount_number = 0)

################################################################################

# 3
# Mean number of servings of fruit and/or vegetables on average per day
fvservings_list_long <- tbls_mn_summary(.variable = fvservings, .cln = fvcln)

# DATABOOK prep
fvservings_tbls <- tbls_list_split(.data = fvservings_list_long, .vars_amount_number = 0)

################################################################################

# FACTSHEET

# 11. Mean number of servings of fruit consumed on average per day
fs_11_fservings_m <- fs_summary(filter(fservings_tbls$m, agerange == "18–69"), c(3,4,5), Males)
fs_11_fservings_w <- fs_summary(filter(fservings_tbls$w, agerange == "18–69"), c(3,4,5), Females)
fs_11_fservings_b <- fs_summary(filter(fservings_tbls$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_11_fservings_joint <- cbind(fs_11_fservings_b,
                              fs_11_fservings_m,
                              fs_11_fservings_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean number of servings of fruit consumed on average per day", .before = 1)
fs_11_fservings_joint

readr::write_excel_csv(fs_11_fservings_joint, here("FactSheet", "11_fs_fservings.csv"))

# 13. Mean number of servings of vegetables consumed on average per day
fs_13_vservings_m <- fs_summary(filter(vservings_tbls$m, agerange == "18–69"), c(3,4,5), Males)
fs_13_vservings_w <- fs_summary(filter(vservings_tbls$w, agerange == "18–69"), c(3,4,5), Females)
fs_13_vservings_b <- fs_summary(filter(vservings_tbls$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_13_vservings_joint <- cbind(fs_13_vservings_b,
                               fs_13_vservings_m,
                               fs_13_vservings_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean number of servings of vegetables consumed on average per day", .before = 1)
fs_13_vservings_joint

readr::write_excel_csv(fs_13_vservings_joint, here("FactSheet", "13_fs_vservings.csv"))

################################################################################







