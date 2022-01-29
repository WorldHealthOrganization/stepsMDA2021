################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Frequency of problems due to someone else's drinking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Afamproblem.R"))

afamproblem_df <- afamproblem(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- afamproblem_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of family/partner problems due to someone else’s drinking 
# during the past 12 months among all respondents
afamproblem_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
afamproblem_c <- 
  tbls_list_split(.data = afamproblem_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################

