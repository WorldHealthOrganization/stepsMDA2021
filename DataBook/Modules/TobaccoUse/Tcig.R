################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Cigarette smoking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tcig.R"))

tcig_df <- tcig(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tcig_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of daily smokers smoking given quantities of manufactured or hand-rolled cigarettes per day
tcig_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tcig_c <- tbls_list_split(.data = tcig_c_list_long, .select_var = c, .vars_amount_number = 5)

################################################################################
################################################################################

