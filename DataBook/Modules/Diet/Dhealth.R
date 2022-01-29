################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Think too much salt can/cannot cause health problems"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dhealth.R"))

dhealth_df <- dhealth(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dhealth_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Think consuming too much salt could cause serious health problem
dhealth_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
dhealth_c <- tbls_list_split(
  .data = dhealth_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) think too much salt can cause health problems")

################################################################################





