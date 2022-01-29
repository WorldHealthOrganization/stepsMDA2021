################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Stopping drinking due to health reasons"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Astopdrink.R"))

astopdrink_df <- astopdrink(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- astopdrink_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Stopping drinking due to health reasons
astopdrink_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
astopdrink_c <- tbls_list_split(
  .data = astopdrink_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) stopped due to health reasons")

################################################################################


