################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Frequency of cleaning teeth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Ofreqclean.R"))

ofreqclean_df <- ofreqclean(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ofreqclean_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Percentage of respondents cleaning their teeth at least once a day
ofreqclean_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
ofreqclean_c <- tbls_list_split(
  .data = ofreqclean_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) daily")

################################################################################

# 2 - Percentage of respondents cleaning their teeth at least twice a day
ofreqclean_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
ofreqclean_d <- tbls_list_split(
  .data = ofreqclean_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) twice daily")

################################################################################


