################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Thinking about quitting due to warnings on cig packages"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPquitting.R"))

tpquitting_df <- tpquitting(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpquitting_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current smokers who saw health warnings on cigarette packages that thought of quitting
tpquitting_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tpquitting_c <- tbls_list_split(
  .data = tpquitting_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) thought about quitting")

