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

source(here("DataBook", "Modules", "TobaccoUse", "functions", "HTSmokefreq.R"))

htsmokefreq_df <- htsmokefreq(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- htsmokefreq_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current daily users among heated tobacco products users
htsmokefreq_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
htsmokefreq_c <- tbls_list_split(
  .data = htsmokefreq_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) current daily user")

################################################################################
################################################################################

