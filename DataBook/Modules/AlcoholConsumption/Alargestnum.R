################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Largest number of drinks in the past 30 days"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Alargestnum.R"))

alargestnum_df <- alargestnum(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- alargestnum_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean maximum number of standard drinks consumed on one occasion in the past 30 days
alargestnum_a8_list_long <- tbls_summary(.mn_pct_md = mn, .variable = a8)

# DATABOOK prep
alargestnum_a8 <- tbls_list_split(.data = alargestnum_a8_list_long, .vars_amount_number = 0)

################################################################################


