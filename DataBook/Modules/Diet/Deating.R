################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Adding salt before or during eating"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Deating.R"))

deating_df <- deating(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- deating_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Add salt always or often before eating or when eating 
deating_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
deating_c <- tbls_list_split(
  .data = deating_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) always or often added salt")

################################################################################

# FACTSHEET

# 15. Percentage who always or often add salt or salty sauce to their food before eating or as they are eating
fs_15_deating_c_m <- fs_summary(filter(deating_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_15_deating_c_w <- fs_summary(filter(deating_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_15_deating_c_b <- fs_summary(filter(deating_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_15_deating_c_joint <- cbind(fs_15_deating_c_b,
                               fs_15_deating_c_m,
                               fs_15_deating_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who always or often add salt or salty sauce to their food before eating or as they are eating", .before = 1)
fs_15_deating_c_joint

readr::write_excel_csv(fs_15_deating_c_joint, here("FactSheet", "15_fs_deating_c.csv"))


################################################################################
################################################################################





