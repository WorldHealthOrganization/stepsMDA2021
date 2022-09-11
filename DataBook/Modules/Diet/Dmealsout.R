################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Controlling salt intake"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dmealsout.R"))

dmealsout_df <- dmealsout(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dmealsout_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Type of oil or fat most often used for meal preparation in household
dmealsout_list_long <- tbls_summary(.mn_pct_md = mn, .variable = d13)

# DATABOOK prep
dmealsout_tbls <- tbls_list_split(.data = dmealsout_list_long, .vars_amount_number = 0)

################################################################################



