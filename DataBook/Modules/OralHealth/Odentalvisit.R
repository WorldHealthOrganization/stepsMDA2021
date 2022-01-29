################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Time since last visit to the dentist"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Odentalvisit.R"))

odentalvisit_df <- odentalvisit(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- odentalvisit_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Percentage of respondents having seen a dentist during the past 12 months
odentalvisit_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
odentalvisit_c <- tbls_list_split(
  .data = odentalvisit_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) saw dentist in last year")

################################################################################

# 2 - Percentage of respondents who have never received dental care
odentalvisit_d_list_long <- tbls_pct_summary(.variable = d)

# DATABOOK prep
odentalvisit_d <- tbls_list_split(
  .data = odentalvisit_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) never received dental care")

################################################################################


