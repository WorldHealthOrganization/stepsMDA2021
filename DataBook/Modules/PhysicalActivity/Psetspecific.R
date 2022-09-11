################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Setting Specific Physical Activity Per Day (in mins)"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P1-P15 script

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data <- cleanrecodep1p15(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Psetspecific.R"))

psetspecific_df <- psetspecific(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- psetspecific_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# MEAN

# 1 - Mean minutes of work-related physical activity on average per day
psetspecific_pworkday_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = pworkday)
# DATABOOK prep
psetspecific_pworkday_mn <- tbls_list_split(
  .data = psetspecific_pworkday_mn_list_long, .vars_amount_number = 0)

# 2 - Mean minutes of transport-related physical activity on average per day
psetspecific_ptravelday_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = ptravelday)
# DATABOOK prep
psetspecific_ptravelday_mn <- tbls_list_split(
  .data = psetspecific_ptravelday_mn_list_long, .vars_amount_number = 0)

# 3 - Mean minutes of recreation-related physical activity on average per day
psetspecific_precday_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = precday)
# DATABOOK prep
psetspecific_precday_mn <- tbls_list_split(
  .data = psetspecific_precday_mn_list_long, .vars_amount_number = 0)

################################################################################

# MEDIAN

# 1 - Median minutes of work-related physical activity on average per day
psetspecific_pworkday_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = pworkday)
# DATABOOK prep
psetspecific_pworkday_md <- tbls_list_split(
  .data = psetspecific_pworkday_md_list_long, .vars_amount_number = 0)

# 2 - Median minutes of transport-related physical activity on average per day
psetspecific_ptravelday_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = ptravelday)
# DATABOOK prep
psetspecific_ptravelday_md <- tbls_list_split(
  .data = psetspecific_ptravelday_md_list_long, .vars_amount_number = 0)

# 3 - Median minutes of recreation-related physical activity on average per day
psetspecific_precday_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = precday)
# DATABOOK prep
psetspecific_precday_md <- tbls_list_split(
  .data = psetspecific_precday_md_list_long, .vars_amount_number = 0)

################################################################################



