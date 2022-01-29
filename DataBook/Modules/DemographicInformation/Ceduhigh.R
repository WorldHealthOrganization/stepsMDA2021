################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Education"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "DemographicInformation", "functions", "Ceduhigh.R"))

ceduhigh_df <- ceduhigh(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ceduhigh_df %>% 
  as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Highest level of education
ceduhigh_c_list_long <- tbls_pct_summary(.variable = c, .fun = summary_pct_unwt)

ceduhigh_c <- tbls_list_split(.data = ceduhigh_c_list_long, .select_var = c, .vars_amount_number = 7)

################################################################################



