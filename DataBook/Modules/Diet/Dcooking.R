################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Adding salt while cooking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dcooking.R"))

dcooking_df <- dcooking(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dcooking_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Add salt always or often before eating or when eating 
dcooking_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
dcooking_c <- tbls_list_split(
  .data = dcooking_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) always or often added salt")

################################################################################












