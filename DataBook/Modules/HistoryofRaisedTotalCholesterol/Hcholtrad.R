################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Raised cholesterol advice or treatment from a traditional healer"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedTotalCholesterol", "functions", "Hcholtrad.R"))

hcholtrad_df <- hcholtrad(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hcholtrad_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Seen a traditional healer for raised cholesterol among those previously diagnosed
hcholtrad_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h15cln)

# DATABOOK prep
hcholtrad_c <- tbls_list_split(
  .data = hcholtrad_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) has seen a traditional healer")

################################################################################

# Currently taking herbal or traditional treatment for raised cholesterol among those previously diagnosed
hcholtrad_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h16cln)

# DATABOOK prep
hcholtrad_d <- tbls_list_split(
  .data = hcholtrad_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking herbal/traditional remedy")

################################################################################


