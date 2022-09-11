################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Avg volume drinking categories among all respondents and among current (past 30 days) drinkers"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Acategories.R"))

acategories_df <- acategories(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- acategories_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# NOTE: the use of "e" variable in scripts instead of c,d,e like in Epi Info
# because e contains men, women and both sexes, and can be used in R functions
# to produce the same results, as individual variables produce in Epi Info

# 1 PART
acategories_high_int_low_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e)


# Drinking at high-end level among all respondents (≥60g of pure alcohol on average 
# per occasion among men and ≥40g of pure alcohol on average per occasion among women)

# MEN % ≥60g 
# WOMEN % ≥40g
# BOTH SEXES % high-end level
acategories_high <- 
  tbls_list_split(.data = acategories_high_int_low_list_long, .select_var = e, 
                  .vars_amount_number = 2, .select_var_val = "1) high-end")

################################################################################

# Drinking at intermediate level among all respondents (40-59.9g of pure alcohol on average 
# per occasion among men and 20-39.9g of pure alcohol on average per occasion among women)

# MEN % 40-59.9g
# WOMEN % 20-39.9g
# BOTH SEXES % intermediate level
acategories_int <- 
  tbls_list_split(.data = acategories_high_int_low_list_long, .select_var = e, 
                  .vars_amount_number = 2, .select_var_val = "2) intermed level")

################################################################################

# Drinking at lower-end level among all respondents (<40g of pure alcohol on average 
# per occasion among men and <20g of pure alcohol on average per occasion among women)

# MEN % <40g
# WOMEN % <20g
# BOTH SEXES % lower-end level
acategories_low <- 
  tbls_list_split(.data = acategories_high_int_low_list_long, .select_var = e, 
                  .vars_amount_number = 2, .select_var_val = "3) lower-end")

################################################################################
################################################################################

# 2 PART
# High-end, intermediate, and lower-end level drinking among current (past 30 days) drinkers
acategories_drinker_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = clndrinker)

# DATABOOK prep
acategories_drinker <- 
  tbls_list_split(.data = acategories_drinker_list_long, .select_var = e, .vars_amount_number = 4) %>% 
  map(~ select(., -c(m4,m_low4,m_upp4)))

################################################################################

