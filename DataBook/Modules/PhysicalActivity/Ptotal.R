################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Total physical activity per day (in mins)"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P1-P15 script

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data <- cleanrecodep1p15(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Ptotal.R"))

ptotal_df <- ptotal(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ptotal_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean minutes of total physical activity on average per day
ptotal_mn_list_long <- tbls_mn_summary(.variable = ptotalday)

# DATABOOK prep
ptotal_mn <- tbls_list_split(.data = ptotal_mn_list_long, .vars_amount_number = 0)

################################################################################

# Median minutes of total physical activity on average per day
ptotal_md_list_long <- tbls_mn_summary(.variable = ptotalday, .fun = pa_summary_md)

# DATABOOK prep
ptotal_md <- tbls_list_split(.data = ptotal_md_list_long, .vars_amount_number = 0)

################################################################################




