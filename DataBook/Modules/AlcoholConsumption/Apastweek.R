################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Drinking in past week" 

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Apastweek.R"))

apastweek_df <- apastweek(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- apastweek_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of alcohol consumption in the past 7 days
apastweek_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
apastweek_c <- 
  tbls_list_split(.data = apastweek_c_list_long, .select_var = c, .vars_amount_number = 5)

################################################################################

# Mean number of standard drinks consumed on average per day in the past 7 days among current drinkers
apastweek_drinksday_list_long <- tbls_mn_summary(.variable = meandrinksperday)

# DATABOOK prep
apastweek_drinksday <- tbls_list_split(.data = apastweek_drinksday_list_long, .vars_amount_number = 0)

################################################################################

