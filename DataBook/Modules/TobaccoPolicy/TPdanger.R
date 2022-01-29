################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Noticing information on TV, newspapers/magazines or radio about dangers of smoking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPdanger.R"))

tpdanger_df <- tpdanger(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpdanger_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# 1 #
# Noticed information in newspapers or magazines about dangers of smoking or that encourages quitting
tpdanger_c_list_long <- tbls_pct_summary(.variable = c, .cln = tp1acln)

# DATABOOK prep
tpdanger_c <- tbls_list_split(
  .data = tpdanger_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) noticed information in newspapers")

################################################################################

# 2 #
# Noticed information on television about dangers of smoking or that encourages quitting
tpdanger_d_list_long <- tbls_pct_summary(.variable = d, .cln = tp1bcln)

# DATABOOK prep
tpdanger_d <- tbls_list_split(
  .data = tpdanger_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) noticed information on TV")

################################################################################

# 3 #
# Noticed information on the radio about dangers of smoking or that encourages quitting
tpdanger_e_list_long <- tbls_pct_summary(.variable = e, .cln = tp1ccln)

# DATABOOK prep
tpdanger_e <- tbls_list_split(
  .data = tpdanger_e_list_long, 
  .select_var = e, .vars_amount_number = 2,
  .select_var_val = "1) noticed information on radio")

################################################################################

# 4 # NOT INCLUDED IN DATABOOK
# Noticed information on the TV or radio about dangers of smoking or that encourages quitting
tpdanger_f_list_long <- tbls_pct_summary(.variable = f, .cln = tp1bcln, .cln_val = 1, 
                                         .cln2 = tp1ccln, .cln2_val = 1)

# DATABOOK prep
tpdanger_f <- tbls_list_split(
  .data = tpdanger_f_list_long, 
  .select_var = f, .vars_amount_number = 2,
  .select_var_val = "1) noticed information on TV or radio")




