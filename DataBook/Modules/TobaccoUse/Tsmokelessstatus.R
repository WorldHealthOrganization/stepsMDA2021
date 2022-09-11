################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Smokeless tobacco use status"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokelessstatus.R"))

tsmokelessstatus_df <- tsmokelessstatus(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokelessstatus_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Current users of smokeless tobacco
tsmokelessstatus_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
tsmokelessstatus_d <- tbls_list_split(
  .data = tsmokelessstatus_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) daily and non-daily users")

################################################################################

# Smokeless tobacco use
tsmokelessstatus_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
tsmokelessstatus_c <- 
  tbls_list_split(.data = tsmokelessstatus_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################
################################################################################



