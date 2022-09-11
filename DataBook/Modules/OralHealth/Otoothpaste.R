################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Percentage of respondents using toothpaste"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Otoothpaste.R"))

otoothpaste_df <- otoothpaste(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- otoothpaste_df %>%
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents using toothpaste among those cleaning their teeth
otoothpaste_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
otoothpaste_c <- tbls_list_split(
  .data = otoothpaste_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) uses toothpaste")

################################################################################







