################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Mean price for 20 cigs"

# Note from EpiInfo: 
# "This program assumes a maximum of 7776 (error code of 7777) for the number of cigarettes 
# purchased and the amount spent on this purchase. If your data uses more or less, 
# please modify the program accordingly or ask the STEPS team for assistance."

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoPolicy", "functions", "TPcost.R"))

tpcost_df <- tpcost(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tpcost_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Average price paid for 20 manufactured cigarettes
tpcost_priceper20cigs_list_long <- tbls_summary(.mn_pct_md = mn, .variable = priceper20cigs)

# DATABOOK prep
tpcost_priceper20cigs <- 
  tbls_list_split(.data = tpcost_priceper20cigs_list_long, .vars_amount_number = 0)

################################################################################

# Monthly expenditure (NOT INCLUDED IN DATABOOK)
tpcost_monthlyexp_list_long <-  
  tbls_summary(.mn_pct_md = mn, .variable = monthlyexp, .cln = cigcln)

# DATABOOK prep
tpcost_monthlyexp <- 
  tbls_list_split(.data = tpcost_monthlyexp_list_long, .vars_amount_number = 0)



