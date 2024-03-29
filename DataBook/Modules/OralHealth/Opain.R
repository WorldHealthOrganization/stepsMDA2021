################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Pain in teeth / mouth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Opain.R"))

opain_df <- opain(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- opain_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage having oral pain or discomfort
opain_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
opain_c <- tbls_list_split(
  .data = opain_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) experienced pain")

################################################################################







