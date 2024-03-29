################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Unpaid work and unemployed"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Cworknotpaid.R"))

cworknotpaid_df <- cworknotpaid(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cworknotpaid_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Unpaid work and unemployed
cworknotpaid_c_list_long <- tbls_summary(.wt_unwt = unwt, .mn_pct_md = pct, .variable = c)

cworknotpaid_c <- tbls_list_split(
  .data = cworknotpaid_c_list_long, .select_var = c, .vars_amount_number = 6)

################################################################################








