################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Initiation and duration of smoking"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokeagetime.R"))

tsmokeagetime_df <- tsmokeagetime(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokeagetime_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# ***DAILY SMOKERS

# Age started smoking, among current daily smokers
tsmokeagetime_initiation_list_long <- 
  tbls_mn_summary(.variable = initiation, .cln2 = t2, .cln2_val = 1)

# DATABOOK prep
tsmokeagetime_initiation <- 
  tbls_list_split(.data = tsmokeagetime_initiation_list_long, .vars_amount_number = 0)

# Forestplot
# library(ggforestplot) # this requires m_se variable in the dataframe and m as numbers
# library(ggforce)
# # MEN, WOMEN, BOTH SEXES
# forestplot(
#   df = tsmokeagetime_initiation_list_long$m_w_b,
#   name = sex,
#   estimate = m,
#   se = m_se,
#   colour = agerange,
#   #shape = agerange,
#   title = "Age started smoking, among current daily smokers",
#   xlab = "Mean age") 

# testplot.mean <- forestplot_steps(
#   .data = tsmokeagetime_initiation_list_long$m_w_b, .title = "test mean", 
#   .xlab = "mean age")

################################################################################

# Duration of smoking, among current daily smokers
tsmokeagetime_duration_list_long <- 
  tbls_mn_summary(.variable = duration, .cln2 = t2, .cln2_val = 1)

# DATABOOK prep
tsmokeagetime_duration <- 
  tbls_list_split(.data = tsmokeagetime_duration_list_long, .vars_amount_number = 0)

################################################################################
################################################################################

# FACTSHEET

# 3. Average age started smoking (years)
fs_tsmokeagetime_initiation_m <- fs_summary(filter(tsmokeagetime_initiation$m, agerange == "18–69"), c(3,4,5), Males)
fs_tsmokeagetime_initiation_w <- fs_summary(filter(tsmokeagetime_initiation$w, agerange == "18–69"), c(3,4,5), Females)
fs_tsmokeagetime_initiation_b <- fs_summary(filter(tsmokeagetime_initiation$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_tsmokeagetime_initiation_joint <- cbind(fs_tsmokeagetime_initiation_b,
                                           fs_tsmokeagetime_initiation_m,
                                           fs_tsmokeagetime_initiation_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Average age started smoking (years)", .before = 1)
fs_tsmokeagetime_initiation_joint

library(readr)
write_excel_csv(fs_tsmokeagetime_initiation_joint, here("FactSheet", "03_fs_initiation.csv"))

################################################################################
################################################################################

