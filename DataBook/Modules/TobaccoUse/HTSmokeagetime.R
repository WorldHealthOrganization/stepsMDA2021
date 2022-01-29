################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Heated tobacco products usage"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "HTSmokeagetime.R"))

htsmokeagetime_df <- htsmokeagetime(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- htsmokeagetime_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean age started using e-cigarettes

htsmokeagetime_initiation_list_long <- 
  tbls_mn_summary(.variable = initiation, .cln2 = htp2, .cln2_val = 1)

# DATABOOK prep
htsmokeagetime_initiation <- 
  tbls_list_split(.data = htsmokeagetime_initiation_list_long, .vars_amount_number = 0)

################################################################################

# Mean duration of using e-cigarettes

htsmokeagetime_duration_list_long <- 
  tbls_mn_summary(.variable = duration, .cln2 = htp2, .cln2_val = 1)

# DATABOOK prep
htsmokeagetime_duration <- 
  tbls_list_split(.data = htsmokeagetime_duration_list_long, .vars_amount_number = 0)

################################################################################


