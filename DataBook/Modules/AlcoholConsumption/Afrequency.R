################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Frequency of alcohol consumption among drinkers"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Afrequency.R"))

afrequency_df <- afrequency(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- afrequency_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Frequency of alcohol consumption in the past 12 months
afrequency_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
afrequency_c <- tbls_list_split(.data = afrequency_c_list_long, .select_var = c, .vars_amount_number = 7) %>% 
  map(~ select(., -c(m7,m_low7,m_upp7))) # removing "never" columns

################################################################################

