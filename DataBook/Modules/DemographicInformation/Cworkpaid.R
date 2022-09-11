################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Employment status"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Cworkpaid.R"))

cworkpaid_df <- cworkpaid(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cworkpaid_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Employment status
cworkpaid_c_list_long <- tbls_summary(.wt_unwt = unwt, .mn_pct_md = pct, .variable = c)

cworkpaid_c <- tbls_list_split(
  .data = cworkpaid_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################






