################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Ethnicity"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Cethnic.R"))

cethnic_df <- cethnic(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- cethnic_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Ethnic group of respondents
cethnic_c_list_long <- tbls_pct_summary(.variable = c, .fun = summary_pct_unwt)

cethnic_c <- tbls_list_split(
  .data = cethnic_c_list_long, .select_var = c, .vars_amount_number = 6)

################################################################################






