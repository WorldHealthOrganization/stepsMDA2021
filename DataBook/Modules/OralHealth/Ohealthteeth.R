################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Health of teeth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Ohealthteeth.R"))

ohealthteeth_df <- ohealthteeth(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ohealthteeth_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents having poor or very poor state of teeth among those having natural teeth
ohealthteeth_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
ohealthteeth_c <- tbls_list_split(
  .data = ohealthteeth_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "2) poor or very poor health")

################################################################################







