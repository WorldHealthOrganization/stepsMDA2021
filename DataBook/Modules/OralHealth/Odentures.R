################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Percentage of respondents with dentures"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Odentures.R"))

odentures_df <- odentures(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- odentures_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents having removable dentures
odentures_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
odentures_c <- tbls_list_split(
  .data = odentures_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) has dentures")

################################################################################

# Percentage of respondents having an upper jaw denture among those having removable dentures
odentures_d_list_long <- tbls_pct_summary(.variable = d, .cln = o6acln, .cln_val = 1)

# DATABOOK prep
odentures_d <- tbls_list_split(
  .data = odentures_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) has upper dentures")


################################################################################

# Percentage of respondents having a lower jaw denture among those having removable dentures
odentures_e_list_long <- tbls_pct_summary(.variable = e, .cln = o6bcln, .cln_val = 1)

# DATABOOK prep
odentures_e <- tbls_list_split(
  .data = odentures_e_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "1) has lower dentures")

################################################################################

# Percentage of respondents having an upper and a lower jaw denture among those having removable dentures
odentures_f_list_long <- tbls_pct_summary(.variable = f, .cln2 = o6acln, .cln2_val = 1, 
                                          .cln3 = o6bcln, .cln3_val = 1)

# DATABOOK prep
odentures_f <- tbls_list_split(
  .data = odentures_f_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "1) has both upper and lower dentures")



