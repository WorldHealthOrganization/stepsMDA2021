################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Smoking Cessation"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tcessation.R"))

tcessation_df <- tcessation(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tcessation_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current smokers who have tried to stop smoking
tcessation_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = stopcln)

# DATABOOK prep
tcessation_c <- tbls_list_split(
  .data = tcessation_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) tried to stop smoking")

################################################################################

# Current smokers who have been advised by doctor to stop smoking
tcessation_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = mdcln)

# DATABOOK prep
tcessation_d <- tbls_list_split(
  .data = tcessation_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) advised to quit")

################################################################################
################################################################################





