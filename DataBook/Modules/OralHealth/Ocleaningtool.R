################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Tool used to clean teeth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Ocleaningtool.R"))

ocleaningtool_df <- ocleaningtool(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- ocleaningtool_df %>%
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents using various tools to clean teeth

# 1 - toothbrush
ocleaningtool_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = o13acln)

# DATABOOK prep
ocleaningtool_a <- tbls_list_split(
  .data = ocleaningtool_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "1) uses toothbrush")

################################################################################

# 2 - wooden tooth-picks
ocleaningtool_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = o13bcln)

# DATABOOK prep
ocleaningtool_b <- tbls_list_split(
  .data = ocleaningtool_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "1) uses wooden tooth-picks")

################################################################################

# 3 - plastic tooth-picks
ocleaningtool_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = o13ccln)

# DATABOOK prep
ocleaningtool_c <- tbls_list_split(
  .data = ocleaningtool_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) uses plastic tooth-picks")

################################################################################

# 4 - Thread (dental floss)
ocleaningtool_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = o13dcln)

# DATABOOK prep
ocleaningtool_d <- tbls_list_split(
  .data = ocleaningtool_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) uses floss")

################################################################################

# 5 - charcoal
ocleaningtool_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = o13ecln)

# DATABOOK prep
ocleaningtool_e <- tbls_list_split(
  .data = ocleaningtool_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) uses charcoal")

################################################################################

# 6 - chewstick
ocleaningtool_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = o13fcln)

# DATABOOK prep
ocleaningtool_f <- tbls_list_split(
  .data = ocleaningtool_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "1) uses chewstick")

################################################################################

# 7 - other
ocleaningtool_g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, .cln = o13gcln)

# DATABOOK prep
ocleaningtool_g <- tbls_list_split(
  .data = ocleaningtool_g_list_long, .select_var = g, 
  .vars_amount_number = 2, .select_var_val = "1) uses other")



