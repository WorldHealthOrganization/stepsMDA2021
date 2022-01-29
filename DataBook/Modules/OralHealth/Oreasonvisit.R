################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Reason for last visit to the dentist"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Oreasonvisit.R"))

oreasonvisit_df <- oreasonvisit(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- oreasonvisit_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Main reason for last visit to the dentist among those who ever visited a dentist
oreasonvisit_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
oreasonvisit_c <- tbls_list_split(
  .data = oreasonvisit_c_list_long, .select_var = c, .vars_amount_number = 5)

################################################################################

# Main reason not to visit a dentist during last 12 months
oreasonvisit_d_list_long <- tbls_pct_summary(.variable = d, .cln = clnox2)

# DATABOOK prep
oreasonvisit_d <- tbls_list_split(
  .data = oreasonvisit_d_list_long, .select_var = d, .vars_amount_number = 5)

################################################################################


