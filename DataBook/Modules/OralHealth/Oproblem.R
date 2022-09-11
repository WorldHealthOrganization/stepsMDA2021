################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Problems due to teeth"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "OralHealth", "functions", "Oproblem.R"))

oproblem_df <- oproblem(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- oproblem_df %>%
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of respondents who experienced any of the following problems during 
# the past 12 months because of the state of their teeth, gums and mouth

# 1 - Percentage of respondents having difficulty in chewing foods during the past 12 months
oproblem_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = o14acln)

# DATABOOK prep
oproblem_a <- tbls_list_split(
  .data = oproblem_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "1) had chewing difficulty")

################################################################################

# 2 - Percentage of respondents having difficulty with speech/trouble pronouncing words during the past 12 months
oproblem_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = o14bcln)

# DATABOOK prep
oproblem_b <- tbls_list_split(
  .data = oproblem_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "1) had speech difficulty")

################################################################################

# 3 - Percentage of respondents feeling tense because of problems with teeth or mouth during the past 12 months
oproblem_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = o14ccln)

# DATABOOK prep
oproblem_c <- tbls_list_split(
  .data = oproblem_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) felt tense")

################################################################################

# 4 - Percentage of respondents being embarrassed because of appearance of teeth during the past 12 months
oproblem_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = o14dcln)

# DATABOOK prep
oproblem_d <- tbls_list_split(
  .data = oproblem_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) felt embarrassed")

################################################################################

# 5 - Percentage of respondents avoiding smiling because of teeth during the past 12 months
oproblem_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = o14ecln)

# DATABOOK prep
oproblem_e <- tbls_list_split(
  .data = oproblem_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "1) avoided smiling")

################################################################################

# 6 - Percentage of respondents with interruptions in sleep during the past 12 months
oproblem_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = o14fcln)

# DATABOOK prep
oproblem_f <- tbls_list_split(
  .data = oproblem_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "1) had interrupted sleep")

################################################################################

# 7 - Percentage of respondents with days not at work because of teeth or mouth during the past 12 months
oproblem_g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, .cln = o14gcln)

# DATABOOK prep
oproblem_g <- tbls_list_split(
  .data = oproblem_g_list_long, .select_var = g, 
  .vars_amount_number = 2, .select_var_val = "1) missed work")

################################################################################

# 8 - Percentage of respondents having difficulty doing usual activities during the past 12 months
oproblem_h_list_long <- tbls_summary(.mn_pct_md = pct, .variable = h, .cln = o14hcln)

# DATABOOK prep
oproblem_h <- tbls_list_split(
  .data = oproblem_h_list_long, .select_var = h, 
  .vars_amount_number = 2, .select_var_val = "1) had difficulty doing activities")

################################################################################

# 9 - Percentage of respondents having been less tolerant of spouse or people close to them during the past 12 months
oproblem_i_list_long <- tbls_summary(.mn_pct_md = pct, .variable = i, .cln = o14icln)

# DATABOOK prep
oproblem_i <- tbls_list_split(
  .data = oproblem_i_list_long, .select_var = i, 
  .vars_amount_number = 2, .select_var_val = "1) was less tolerant")

################################################################################

# 10 - Percentage of respondents having reduced participation in social activities during the past 12 months
oproblem_j_list_long <- tbls_summary(.mn_pct_md = pct, .variable = j, .cln = o14jcln)

# DATABOOK prep
oproblem_j <- tbls_list_split(
  .data = oproblem_j_list_long, .select_var = j, 
  .vars_amount_number = 2, .select_var_val = "1) reduced social activities")


