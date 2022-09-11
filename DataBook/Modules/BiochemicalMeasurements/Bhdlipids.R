################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "High density lipoprotien (HDL)"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "BiochemicalMeasurements", "functions", "Bhdlipids.R"))

bhdlipids_df <- bhdlipids(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- bhdlipids_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep3, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# MEN
# Percentage of respondents with HDL <1.03mmol/L or <40 mg/dl
# NOTE: only values for men are used for reporting in Data Book
bhdlipids_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
bhdlipids_c <- 
  tbls_list_split(
    .data = bhdlipids_c_list_long, .select_var = c, 
    .vars_amount_number = 2, .select_var_val = "HDL <1.03mmol/L")

################################################################################

# WOMEN
# Percentage of respondents with HDL <1.29mmol/L or <50 mg/dl
# NOTE: only values for women are used for reporting in Data Book
bhdlipids_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
bhdlipids_d <- 
  tbls_list_split(
    .data = bhdlipids_d_list_long, .select_var = d, 
    .vars_amount_number = 2, .select_var_val = "HDL <1.29mmol/L")

################################################################################

# MEN, WOMEN, BOTH SEXES
# Mean HDL (mmol/L)
bhdlipids_b17_list_long <- tbls_summary(.mn_pct_md = mn, .variable = b17)

# DATABOOK prep
bhdlipids_b17 <- 
  tbls_list_split(.data = bhdlipids_b17_list_long, .vars_amount_number = 0)

################################################################################

# MEN, WOMEN, BOTH SEXES
# Mean HDL (mg/dl)
bhdlipids_b17mg_list_long <- tbls_summary(.mn_pct_md = mn, .variable = b17mg)

# DATABOOK prep
bhdlipids_b17mg <- 
  tbls_list_split(.data = bhdlipids_b17mg_list_long, .vars_amount_number = 0)

################################################################################






