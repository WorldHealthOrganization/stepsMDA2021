################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Tobacco use status"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tdailyuser.R"))

tdailyuser_df <- tdailyuser(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tdailyuser_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current tobacco users
tdailyuser_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tdailyuser_c <- tbls_list_split(
  .data = tdailyuser_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) current tobacco user")

################################################################################

# Daily tobacco users
tdailyuser_d_list_long <- tbls_pct_summary(.variable = d)

# DATABOOK prep
tdailyuser_d <- tbls_list_split(
  .data = tdailyuser_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) current daily user")

################################################################################
################################################################################

