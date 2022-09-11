################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Eating salty processed food"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dprocessed.R"))

dprocessed_df <- dprocessed(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dprocessed_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Always or often consume processed food high in salt
dprocessed_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
dprocessed_c <- tbls_list_split(
  .data = dprocessed_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) always or often consume salty processed food")

################################################################################

# FACTSHEET

# 16. Percentage who always or often eat processed foods high in salt
fs_16_dprocessed_c_m <- fs_summary(filter(dprocessed_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_16_dprocessed_c_w <- fs_summary(filter(dprocessed_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_16_dprocessed_c_b <- fs_summary(filter(dprocessed_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_16_dprocessed_c_joint <- cbind(fs_16_dprocessed_c_b,
                                  fs_16_dprocessed_c_m,
                                  fs_16_dprocessed_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who always or often eat processed foods high in salt", .before = 1)

readr::write_excel_csv(fs_16_dprocessed_c_joint, here("FactSheet", "16_fs_dprocessed_c.csv"))


################################################################################




