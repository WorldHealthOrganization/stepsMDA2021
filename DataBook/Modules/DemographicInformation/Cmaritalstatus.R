################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Marital Status"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Cmaritalstatus.R"))

cmaritalstatus_df <- cmaritalstatus(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cmaritalstatus_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Marital status
cmaritalstatus_c_list_long <- tbls_summary(.wt_unwt = unwt, .mn_pct_md = pct, .variable = c)

cmaritalstatus_c <- tbls_list_split(
  .data = cmaritalstatus_c_list_long, .select_var = c, .vars_amount_number = 6)

################################################################################



