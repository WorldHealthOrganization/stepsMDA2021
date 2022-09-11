################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Percentage of ex-daily smokers in the population"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokeexdaily.R"))

tsmokeexdaily_df <- tsmokeexdaily(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokeexdaily_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Former daily smokers (who don’t smoke currently) among all respondents
tsmokeexdaily_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
tsmokeexdaily_c <- tbls_list_split(
  .data = tsmokeexdaily_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "Ex-daily smoker")

################################################################################

# Former daily smokers (who don’t smoke currently) among ever daily smokers
tsmokeexdaily_everdaily_c_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c, 
                                                    .cln2 = everdaily, .cln2_val = 1)

# DATABOOK prep
tsmokeexdaily_everdaily_c <- tbls_list_split(
  .data = tsmokeexdaily_everdaily_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "Ex-daily smoker")

################################################################################

# Mean years since cessation
tsmokeexdaily_stop_list_long <- 
  tbls_summary(.mn_pct_md = mn, .variable = stop, .cln = t1, .cln_val = 2, 
               .cln2 = t8, .cln2_val = 1, .cln3 = stopcln, .cln3_val = 1)

# DATABOOK prep
tsmokeexdaily_stop <- tbls_list_split(.data = tsmokeexdaily_stop_list_long, .vars_amount_number = 0)

################################################################################
################################################################################




