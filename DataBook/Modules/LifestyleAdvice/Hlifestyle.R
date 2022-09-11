################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Lifestyle advice"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "LifestyleAdvice", "functions", "Hlifestyle.R"))

hlifestyle_df <- hlifestyle(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hlifestyle_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Advised by doctor or health worker to quit using tobacco or don’t start
hlifestyle_a_list_long <- tbls_summary(.mn_pct_md = pct, .variable = a, .cln = h20acln)
# DATABOOK prep
hlifestyle_a <- tbls_list_split(
  .data = hlifestyle_a_list_long, .select_var = a, 
  .vars_amount_number = 2, .select_var_val = "advised to quit/not start tob")


# 2 - Advised by doctor or health worker to reduce salt in the diet
hlifestyle_b_list_long <- tbls_summary(.mn_pct_md = pct, .variable = b, .cln = h20bcln)
# DATABOOK prep
hlifestyle_b <- tbls_list_split(
  .data = hlifestyle_b_list_long, .select_var = b, 
  .vars_amount_number = 2, .select_var_val = "advised to reduce salt")


# 3 - Advised by doctor or health worker to eat at least five servings of fruit and/or vegetables each day
hlifestyle_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h20ccln)
# DATABOOK prep
hlifestyle_c <- tbls_list_split(
  .data = hlifestyle_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "advised to eat more fruit/veg")


# 4 - Advised by doctor or health worker to reduce fat in the diet
hlifestyle_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h20dcln)
# DATABOOK prep
hlifestyle_d <- tbls_list_split(
  .data = hlifestyle_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "advised to reduce fat in diet")


# 5 - Advised by doctor or health worker to start or do more physical activity
hlifestyle_e_list_long <- tbls_summary(.mn_pct_md = pct, .variable = e, .cln = h20ecln)
# DATABOOK prep
hlifestyle_e <- tbls_list_split(
  .data = hlifestyle_e_list_long, .select_var = e, 
  .vars_amount_number = 2, .select_var_val = "advised to do more physical activity")


# 6 - Advised by doctor or health worker to maintain a healthy body weight or to lose weight
hlifestyle_f_list_long <- tbls_summary(.mn_pct_md = pct, .variable = f, .cln = h20fcln)
# DATABOOK prep
hlifestyle_f <- tbls_list_split(
  .data = hlifestyle_f_list_long, .select_var = f, 
  .vars_amount_number = 2, .select_var_val = "advised to maintain/lose weight")


# 7 - Advised by doctor or health worker to reduce sugary beverages in diet 
hlifestyle_g_list_long <- tbls_summary(.mn_pct_md = pct, .variable = g, .cln = h20gcln)
# DATABOOK prep
hlifestyle_g <- tbls_list_split(
  .data = hlifestyle_g_list_long, .select_var = g, 
  .vars_amount_number = 2, .select_var_val = "advised to reduce sugary beverages in diet")


################################################################################


