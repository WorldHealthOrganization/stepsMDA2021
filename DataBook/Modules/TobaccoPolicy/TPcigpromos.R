################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Noticing cigarette promotions"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPcigpromos.R"))

tpcigpromos_df <- tpcigpromos(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpcigpromos_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 - Noticed free samples of cigarettes
tpcigpromos_a_list_long <- tbls_pct_summary(.variable = a, .cln = tp3acln)

# DATABOOK prep
tpcigpromos_a <- tbls_list_split(
  .data = tpcigpromos_a_list_long, 
  .select_var = a, .vars_amount_number = 2,
  .select_var_val = "1) noticed free samples")

################################################################################

# 2 - Noticed sale prices on cigarettes
tpcigpromos_b_list_long <- tbls_pct_summary(.variable = b, .cln = tp3bcln)

# DATABOOK prep
tpcigpromos_b <- tbls_list_split(
  .data = tpcigpromos_b_list_long, 
  .select_var = b, .vars_amount_number = 2,
  .select_var_val = "1) noticed cigs on sale")

################################################################################

# 3 - Noticed coupons for cigarettes
tpcigpromos_c_list_long <- tbls_pct_summary(.variable = c, .cln = tp3ccln)

# DATABOOK prep
tpcigpromos_c <- tbls_list_split(
  .data = tpcigpromos_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) noticed cig coupons")

################################################################################

# 4 - Noticed free gifts or special discount offers on other products when buying cigarettes
tpcigpromos_d_list_long <- tbls_pct_summary(.variable = d, .cln = tp3dcln)

# DATABOOK prep
tpcigpromos_d <- tbls_list_split(
  .data = tpcigpromos_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) noticed free gifts")

################################################################################

# 5 - Noticed clothing or other items with a cigarette brand name or logo
tpcigpromos_e_list_long <- tbls_pct_summary(.variable = e, .cln = tp3ecln)

# DATABOOK prep
tpcigpromos_e <- tbls_list_split(
  .data = tpcigpromos_e_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "1) noticed branded clothing")

################################################################################

# 6 - Noticed cigarette promotions in the mail
tpcigpromos_f_list_long <- tbls_pct_summary(.variable = f, .cln = tp3fcln)

# DATABOOK prep
tpcigpromos_f <- tbls_list_split(
  .data = tpcigpromos_f_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "1) noticed mail promos")

################################################################################

# 7 - Noticed any promotions (NOT INCLUDED IN DATABOOK)
tpcigpromos_g_list_long <- tbls_pct_summary(.variable = g, .cln = allcln)

# DATABOOK prep
tpcigpromos_g <- tbls_list_split(
  .data = tpcigpromos_g_list_long, 
  .select_var = g, .vars_amount_number = 2,
  .select_var_val = "1) noticed any promotion")



