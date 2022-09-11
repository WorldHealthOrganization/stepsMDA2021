################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# NOTE: THIS SCRIPT COVERS TWO PROGRAMS (MEANS & MEDIANS)

# "Sedentary Time on a Typical Day" 
# In median programs psu and stratum variables aren't used

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P16 script

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP16.R"))

data <- cleanrecodep16(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Psedentary.R"))

psedentary_df <- psedentary(data)

################################################################################

library(srvyr)

# Specifying design for MEANS

STEPSClean <- psedentary_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# NOTE HERE THE USE OF FUNCTIONS SPECIFIC FOR PHYSICAL ACTIVITY

# Minutes spent in sedentary activities on average per day
# MEAN MINUTES
psedentary_mn_list_long <- tbls_summary(.mn_pct_md = mn, .variable = p16)
# DATABOOK prep
psedentary_mn <- tbls_list_split(
  .data = psedentary_mn_list_long, .vars_amount_number = 0)

# MEDIAN MINUTES
psedentary_md_list_long <- tbls_summary(.mn_pct_md = md, .variable = p16)
# DATABOOK prep
psedentary_md <- tbls_list_split(
  .data = psedentary_md_list_long, .vars_amount_number = 0)

################################################################################

