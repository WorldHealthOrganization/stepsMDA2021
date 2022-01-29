################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Average Hip Circumference"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mhip.R"))

mhip_df <- mhip(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mhip_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

mhip_list_long <- tbls_mn_summary(.variable = m15, .cln = m15cln)

# DATABOOK prep
mhip <- tbls_list_split(.data = mhip_list_long, .vars_amount_number = 0)

################################################################################


