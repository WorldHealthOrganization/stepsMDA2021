################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Drinking occasions in the past 30 days"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aoccasions.R"))

aoccasions_df <- aoccasions(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aoccasions_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean number of drinking occasions in the past 30 days among current (past 30 days) drinkers
aoccasions_a6_list_long <- tbls_mn_summary(.variable = a6)

# DATABOOK prep
aoccasions_a6 <- tbls_list_split(.data = aoccasions_a6_list_long, .vars_amount_number = 0)

################################################################################


