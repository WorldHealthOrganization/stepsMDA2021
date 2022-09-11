################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Number of natural teeth remaining in mouth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Onatural.R"))

onatural_df <- onatural(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- onatural_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents having poor or very poor state of teeth among those having natural teeth
onatural_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
onatural_c <- tbls_list_split(
  .data = onatural_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################







