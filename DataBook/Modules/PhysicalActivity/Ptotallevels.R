################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Levels of total physical activity"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P1-P15 script

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data <- cleanrecodep1p15(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Ptotallevels.R"))

ptotallevels_df <- ptotallevels(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ptotallevels_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Level of total physical activity according to former recommendations
ptotallevels_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
ptotallevels_c <- tbls_list_split(
  .data = ptotallevels_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################











