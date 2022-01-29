################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Frequency of being unable to stop drinking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Anotabletostop.R"))

anotabletostop_df <- anotabletostop(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- anotabletostop_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of not being able to stop drinking once started 
# during the past 12 months among past 12 month drinkers
anotabletostop_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
anotabletostop_c <- 
  tbls_list_split(.data = anotabletostop_c_list_long, .select_var = c, .vars_amount_number = 3)

################################################################################


