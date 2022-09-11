################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Frequency of failing to do what was expected"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Afailexpected.R"))

afailexpected_df <- afailexpected(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- afailexpected_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of failing to do what was normally expected from you during 
# the past 12 months among past 12 month drinkers
afailexpected_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
afailexpected_c <- 
  tbls_list_split(.data = afailexpected_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################

