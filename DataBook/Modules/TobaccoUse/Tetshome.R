################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Exposure to ETS at home" 

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tetshome.R"))

tetshome_df <- tetshome(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tetshome_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Exposed to second-hand smoke in home during the past 30 days
tetshome_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
tetshome_c <- tbls_list_split(
  .data = tetshome_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "exposed at home")

################################################################################
################################################################################







