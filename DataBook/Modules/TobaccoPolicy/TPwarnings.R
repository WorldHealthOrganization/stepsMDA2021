################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Noticing warnings on cig packages"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPwarnings.R"))

tpwarnings_df <- tpwarnings(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpwarnings_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current smokers who noticed health warnings on cigarette packages
tpwarnings_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tpwarnings_c <- tbls_list_split(
  .data = tpwarnings_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) noticed warnings")

