################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Exposure to ETS in the workplace" 

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tetswork.R"))

tetswork_df <- tetswork(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tetswork_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Exposed to second-hand smoke in the workplace during the past 30 days
tetswork_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tetswork_c <- tbls_list_split(
  .data = tetswork_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "exposed at work")

################################################################################
################################################################################




