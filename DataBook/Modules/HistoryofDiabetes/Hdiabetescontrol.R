################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Diabetes measurement, diagnosis and treatment"
# Additional question on control 

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofDiabetes", "functions", "Hdiabetescontrol.R"))

hdiabetescontrol_df <- hdiabetescontrol(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hdiabetescontrol_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Received at least 2 HbA1C (glycated haemoglobin) tests in 
# the past year as part of diabetes control
hdiabetescontrol_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = hx1cln)

# DATABOOK prep
hdiabetescontrol_c <- tbls_list_split(
  .data = hdiabetescontrol_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) received at least 2 HbA1C tests")

################################################################################

# Last time eyes were examined as part of diabetes control
hdiabetescontrol_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = hx2cln)

# DATABOOK prep
hdiabetescontrol_d <- tbls_list_split(
  .data = hdiabetescontrol_d_list_long, .select_var = d, .vars_amount_number = 3)

################################################################################

# Last time feet were examined as part of diabetes control
hdiabetescontrol_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = hx3cln)

# DATABOOK prep
hdiabetescontrol_e <- tbls_list_split(
  .data = hdiabetescontrol_e_list_long, .select_var = e, .vars_amount_number = 3)

################################################################################



