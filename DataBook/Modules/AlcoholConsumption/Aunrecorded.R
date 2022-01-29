################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Consumption of unrecorded alcohol"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aunrecorded.R"))

aunrecorded_df <- aunrecorded(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aunrecorded_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Consumption of unrecorded alcohol
aunrecorded_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
aunrecorded_c <- tbls_list_split(
  .data = aunrecorded_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "consumed unrecorded alcohol")

################################################################################


