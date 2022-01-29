################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Blood pressure measurement, diagnosis and medication"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "HistoryofRaisedBloodPressure", "functions", "Hbloodpressure.R"))

hbloodpressure_df <- hbloodpressure(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hbloodpressure_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Blood pressure measurement and diagnosis
hbloodpressure_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
hbloodpressure_c <- tbls_list_split(
  .data = hbloodpressure_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################

# Currently taking drugs (medication) for raised blood pressure 
# prescribed by doctor or health worker among those diagnosed
hbloodpressure_d_list_long <- tbls_pct_summary(.variable = d, .cln = h3cln)

# DATABOOK prep
hbloodpressure_d <- tbls_list_split(
  .data = hbloodpressure_d_list_long, .select_var = d, 
  .vars_amount_number = 2, .select_var_val = "1) taking meds")

################################################################################
################################################################################
