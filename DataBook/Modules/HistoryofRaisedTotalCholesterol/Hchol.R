################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Cholesterol measurement, diagnosis and medication"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedTotalCholesterol", "functions", "Hchol.R"))

hchol_df <- hchol(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hchol_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Total cholesterol measurement and diagnosis
hchol_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
hchol_c <- tbls_list_split(
  .data = hchol_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking oral treatment (medication) prescribed for 
# raised total cholesterol among those previously diagnosed
hchol_d_list_long <- tbls_pct_summary(.variable = d, .cln = h14cln)

# DATABOOK prep
hchol_d <- tbls_list_split(
  .data = hchol_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################
################################################################################
