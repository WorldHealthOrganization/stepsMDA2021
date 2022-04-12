################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# DEPRESSION
# GENERATE DEPRESSION VARIABLE USING WORLD HEALTH SURVEY (WHS) METHOD
# CHRONIC CONDITIONS ALGORITHM - USING WHS BASED ON RESULTS FROM DIPS & 
# WHS SENSITIVITY & SPECIFICITY

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "DepressionModuleWHS", "functions", "WHSdepression.R"))

whs_depression_df <- whs_depression(data)

################################################################################

# Specifying design for WHS algorithm

library(srvyr)

STEPSClean <- whs_depression_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Prevalence of depressive symptoms using established WHS algorithm
# “symptom + algorithm-based prevalence of depression.”
whs_depression_c_list_long <- tbls_pct_summary(.variable = c, .cln = FALSE, .cln_val = FALSE)

# DATABOOK prep
whs_depression_c <- tbls_list_split(
  .data = whs_depression_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "Yes")

################################################################################

# 2 - ADDITIONAL ANALYSIS, INCLUDING ALL RESPONDENTS
### Self-reported prevalence of depression (question code DA1) among:
### a) Those with “symptom + algorithm-based prevalence of depression.” -> depression == 1
### They were told by a physician that they have depression and they have symptoms 

# DA1 - FROM DEPRESSED BASED ON WHS ALGORITHM
da1_d_whs_list_long <- tbls_pct_summary(
  .variable = d, .cln = depression, .cln2 = da1cln, .cln2_val = 1)

# DATABOOK prep
da1_d_whs <- tbls_list_split(
  .data = da1_d_whs_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) told")

################################################################################

# b) All respondents
# "Told by a doctor or health worker that they have depression"
### Self-reported prevalence of depression (question code DA1) among: 
### From all respondents is indicated as "all"

# DA1 - FROM ALL RESPONDENTS
da1_d_all_list_long <- tbls_pct_summary(.variable = d, .cln = da1cln)

# DATABOOK prep
da1_d_all <- tbls_list_split(
  .data = da1_d_all_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) told")

################################################################################

# Have been taking medications or other treatment for depression in the last 12 months
# NOTE: DA4 is asked when DA1 is "Yes" == 1
# DA4 - FROM ALL RESPONDENTS WHO ANSWERED DA1 "YES"
da4_e_all_list_long <- tbls_pct_summary(.variable = e, .cln = da4cln)

# DATABOOK prep
da4_e_all <- tbls_list_split(
  .data = da4_e_all_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "1) on medication")

################################################################################

# Have been taking medications or other treatment for depression in the last 2 weeks
# NOTE: DA5 is asked when DA4 is "Yes" = 1
# DA5 - FROM ALL RESPONDENTS WHO ANSWERED DA4 "YES"
da5_f_all_list_long <- tbls_pct_summary(.variable = f, .cln = da5cln)

# DATABOOK prep
da5_f_all <- tbls_list_split(
  .data = da5_f_all_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "1) on medication")

################################################################################

# EXTRA
# DA22 -  FROM DEPRESSED BASED ON WHS ALGORITHM (who answered yes to DA6 or DA7 or DA8)
# Thought of death, or wished they were dead, during the last 12 months 
# among those that are depressed based on the WHS algorithm
da22_g_whs_list_long <- tbls_pct_summary(
  .variable = g, .cln = depression, .cln2 = da22cln, .cln2_val = 1)

# DATABOOK prep
da22_g_whs <- tbls_list_split(
  .data = da22_g_whs_list_long, 
  .select_var = g, .vars_amount_number = 2,
  .select_var_val = "1) thought")






