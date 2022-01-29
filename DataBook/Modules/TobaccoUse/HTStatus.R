################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Heated tobacco products usage"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "HTStatus.R"))

htstatus_df <- htstatus(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- htstatus_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of current users of heated tobacco products
htstatus_d_list_long <- tbls_pct_summary(.variable = d)

# DATABOOK prep
htstatus_d <- tbls_list_split(
  .data = htstatus_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) daily and non-daily users")

################################################################################

# Heated tobacco products using status
htstatus_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
htstatus_c <- 
  tbls_list_split(.data = htstatus_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################
################################################################################

