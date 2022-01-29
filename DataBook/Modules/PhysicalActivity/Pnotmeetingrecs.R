################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Not meeting WHO recommendations on physical activity for health"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P1-P15 function

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data <- cleanrecodep1p15(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Pnotmeetingrecs.R"))

pnotmeetingrecs_df <- pnotmeetingrecs(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- pnotmeetingrecs_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Not meeting WHO recommendations on physical activity for health
pnotmeetingrecs_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
pnotmeetingrecs_c <- tbls_list_split(
  .data = pnotmeetingrecs_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) doesn't meet recs")

################################################################################

# EXTRA PART - 1200 METs
# Not meeting WHO recommendations on physical activity for health
# Proportion of the population engaged in physical activity with additional health benefits 
# (physical activity ≥ 1200 MET-minutes/week). Total and data by gender and/or age group.
pnotmeetingrecs_mets1200_list_long <- tbls_pct_summary(.variable = mets1200)

# DATABOOK prep
pnotmeetingrecs_mets1200 <- tbls_list_split(
  .data = pnotmeetingrecs_mets1200_list_long, 
  .select_var = mets1200, .vars_amount_number = 2,
  .select_var_val = "2) meets recs")

################################################################################

# FACTSHEET

# 17. Percentage with insufficient physical activity (defined as < 150 minutes of 
# moderate-intensity activity per week, or equivalent)*
# 
# * For complete definitions of insufficient physical activity, refer to the GPAQ 
# Analysis Guide (http://www.who.int/chp/steps/GPAQ/en/index.html) or to the 
# WHO Global recommendations on physical activity for health 
# (http://www.who.int/dietphysicalactivity/factsheet_recommendations/en/index.html)

fs_17_pnotmeetingrecs_c_m <- fs_summary(filter(pnotmeetingrecs_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_17_pnotmeetingrecs_c_w <- fs_summary(filter(pnotmeetingrecs_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_17_pnotmeetingrecs_c_b <- fs_summary(filter(pnotmeetingrecs_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_17_pnotmeetingrecs_c_joint <- cbind(fs_17_pnotmeetingrecs_c_b,
                                       fs_17_pnotmeetingrecs_c_m,
                                       fs_17_pnotmeetingrecs_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage with insufficient physical activity (defined as < 150 minutes of moderate-intensity activity per week, or equivalent)*", .before = 1)
fs_17_pnotmeetingrecs_c_joint

readr::write_excel_csv(fs_17_pnotmeetingrecs_c_joint, here("FactSheet", "17_fs_pnotmeetingrecs_c.csv"))


################################################################################



