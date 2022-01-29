################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Importance of lowering salt in diet"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dlower.R"))

dlower_df <- dlower(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dlower_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Importance of lowering salt in diet
dlower_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
dlower_c <- 
  tbls_list_split(.data = dlower_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################
################################################################################





