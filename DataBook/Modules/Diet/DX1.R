################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Drinking sugared soft drinks"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "DX1.R"))

dx1func_df <- dx1func(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dx1func_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Description: How often people drink sugared beverages, excluding light, diet and non-sugar beverages.   
# Drinking sugared soft drinks
dx1_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
dx1_c <- tbls_list_split(
  .data = dx1_c_list_long, .select_var = c, .vars_amount_number = 5)

################################################################################
################################################################################


