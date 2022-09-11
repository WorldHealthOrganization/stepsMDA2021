################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Blood pressure advice or treatment from a traditional healer"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedBloodPressure", "functions", "Hraisedbptrad.R"))

hraisedbptrad_df <- hraisedbptrad(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hraisedbptrad_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Seen a traditional healer among those previously diagnosed
hraisedbptrad_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, .cln = h4cln)

# DATABOOK prep
hraisedbptrad_c <- tbls_list_split(
  .data = hraisedbptrad_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) has seen a traditional healer")

################################################################################

# Currently taking drugs (medication) for raised blood pressure 
# prescribed by doctor or health worker among those diagnosed
hraisedbptrad_d_list_long <- tbls_summary(.mn_pct_md = pct, .variable = d, .cln = h5cln)

# DATABOOK prep
hraisedbptrad_d <- tbls_list_split(
  .data = hraisedbptrad_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking herbal/traditional remedy")

################################################################################
################################################################################
