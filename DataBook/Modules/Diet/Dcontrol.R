################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Controlling salt intake"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dcontrol.R"))

dcontrol_df <- dcontrol(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dcontrol_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Limit consumption of processed foods
dcontrol_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = d11acln)

# DATABOOK prep
dcontrol_a <- tbls_list_split(
  .data = dcontrol_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "1) limit consumption of processed foods")

################################################################################

# 2 - Look at the salt or sodium content on food labels
dcontrol_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = d11bcln)

# DATABOOK prep
dcontrol_b <- tbls_list_split(
  .data = dcontrol_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "1) looked at salt labels on food")

################################################################################

# 3 - Buy low salt/sodium alternatives
dcontrol_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = d11ccln)

# DATABOOK prep
dcontrol_c <- tbls_list_split(
  .data = dcontrol_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) bought low salt alternatives")

################################################################################

# 4 - Use spices other than salt when cooking
dcontrol_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = d11dcln)

# DATABOOK prep
dcontrol_d <- tbls_list_split(
  .data = dcontrol_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) used other spices when cooking")

################################################################################

# 5 - Avoid eating foods prepared outside of a home
dcontrol_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = d11ecln)

# DATABOOK prep
dcontrol_e <- tbls_list_split(
  .data = dcontrol_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) avoided eating out")

################################################################################

# 6 - Do other things specifically to control your salt intake
dcontrol_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = d11fcln)

# DATABOOK prep
dcontrol_f <- tbls_list_split(
  .data = dcontrol_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "1) did some other behavior")

################################################################################
################################################################################




