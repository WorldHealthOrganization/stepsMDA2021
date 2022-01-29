################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Diabetes advice from a traditional healer"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofDiabetes", "functions", "Hdiabetestrad.R"))

hdiabetestrad_df <- hdiabetestrad(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hdiabetestrad_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Seen a traditional healer for diabetes among those previously diagnosed
hdiabetestrad_c_list_long <- tbls_pct_summary(.variable = c, .cln = h10cln)

# DATABOOK prep
hdiabetestrad_c <- tbls_list_split(
  .data = hdiabetestrad_c_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) has seen a traditional healer")

################################################################################

# Currently taking herbal or traditional treatment for diabetes among those previously diagnosed
hdiabetestrad_d_list_long <- tbls_pct_summary(.variable = d, .cln = h11cln)

# DATABOOK prep
hdiabetestrad_d <- tbls_list_split(
  .data = hdiabetestrad_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking herbal/traditional remedy")

################################################################################






