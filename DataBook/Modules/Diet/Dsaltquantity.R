################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Self-reported quantity of salt consumed"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "Diet", "functions", "Dsaltquantity.R"))

dsaltquantity_df <- dsaltquantity(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- dsaltquantity_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Think they consume far too much or too much salt
dsaltquantity_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
dsaltquantity_c <- tbls_list_split(
  .data = dsaltquantity_c_list_long, 
  .select_var = c, 
  .vars_amount_number = 2, 
  .select_var_val = "1) eat far too much or too much salt")

################################################################################

# Self-reported quantity of salt consumed
dsaltquantity_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d)

# DATABOOK prep
dsaltquantity_d <- 
  tbls_list_split(.data = dsaltquantity_d_list_long, .select_var = d, .vars_amount_number = 5)

################################################################################









