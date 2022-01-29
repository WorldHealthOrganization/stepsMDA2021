################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "E-cigarettes usage"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "ETSmokeagetime.R"))

etsmokeagetime_df <- etsmokeagetime(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- etsmokeagetime_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean age started using e-cigarettes

etsmokeagetime_initiation_list_long <- 
  tbls_mn_summary(.variable = initiation, .cln2 = et2, .cln2_val = 1)

# DATABOOK prep
etsmokeagetime_initiation <- 
  tbls_list_split(.data = etsmokeagetime_initiation_list_long, .vars_amount_number = 0)

################################################################################

# Mean duration of using e-cigarettes

etsmokeagetime_duration_list_long <- 
  tbls_mn_summary(.variable = duration, .cln2 = et2, .cln2_val = 1)

# DATABOOK prep
etsmokeagetime_duration <- 
  tbls_list_split(.data = etsmokeagetime_duration_list_long, .vars_amount_number = 0)

################################################################################


