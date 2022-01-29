################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Percentage of respondents eating X servings of fruit/veg on avg per day"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dfiveormore.R"))

dfiveormore_df <- dfiveormore(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dfiveormore_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Number of servings of fruit and/or vegetables on average per day
dfiveormore_d_list_long <- tbls_pct_summary(.variable = d)

# DATABOOK prep
dfiveormore_d <- tbls_list_split(.data = dfiveormore_d_list_long, .select_var = d, .vars_amount_number = 4)

################################################################################

# Less than five servings of fruit and/or vegetables on average per day 
dfiveormore_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
dfiveormore_c <- tbls_list_split(
  .data = dfiveormore_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) <5 servings of fruit/veg on avg per day")

################################################################################

# FACTSHEET

# 14. Percentage who ate less than 5 servings of fruit and/or vegetables on average per day
fs_14_dfiveormore_c_m <- fs_summary(filter(dfiveormore_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_14_dfiveormore_c_w <- fs_summary(filter(dfiveormore_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_14_dfiveormore_c_b <- fs_summary(filter(dfiveormore_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_14_dfiveormore_c_joint <- cbind(fs_14_dfiveormore_c_b,
                                   fs_14_dfiveormore_c_m,
                                   fs_14_dfiveormore_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who ate less than 5 servings of fruit and/or vegetables on average per day", .before = 1)
fs_14_dfiveormore_c_joint

readr::write_excel_csv(fs_14_dfiveormore_c_joint, here("FactSheet", "14_fs_dfiveormore_c.csv"))


################################################################################
################################################################################



