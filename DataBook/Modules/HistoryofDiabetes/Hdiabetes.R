################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Diabetes measurement, diagnosis and treatment"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofDiabetes", "functions", "Hdiabetes.R"))

hdiabetes_df <- hdiabetes(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hdiabetes_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Blood sugar measurement and diagnosis
hdiabetes_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
hdiabetes_c <- tbls_list_split(
  .data = hdiabetes_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking drugs (medication) prescribed for diabetes among those previously diagnosed
hdiabetes_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h8cln)

# DATABOOK prep
hdiabetes_d <- tbls_list_split(
  .data = hdiabetes_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################

# Currently taking insulin prescribed for diabetes among those previously diagnosed
hdiabetes_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = h9cln)

# DATABOOK prep
hdiabetes_e <- tbls_list_split(
  .data = hdiabetes_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) taking insulin")

################################################################################



