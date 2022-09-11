################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Heart Rate"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalMeasurements", "functions", "Mheartrate.R"))

mheartrate_df <- mheartrate(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- mheartrate_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep2, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# % of those with heart rate over 100 ("beats per minute over 100") # NOT INCLUDED IN DATA BOOK
# mheartrate_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)
# # DATABOOK prep
# mheartrate_c <- tbls_list_split(
#   .data = mheartrate_c_list_long, .select_var = c, 
#   .vars_amount_number = 2, .select_var_val = "beats per minute over 100")

################################################################################

# Mean heart rate (beats per minute)  # INCLUDED IN DATA BOOK
mheartrate_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = rate)

# DATABOOK prep
mheartrate_mn <- 
  tbls_list_split(.data = mheartrate_mn_list_long, .vars_amount_number = 0)

################################################################################


