################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Smoking Status"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmokestatus.R"))

tsmokestatus_df <- tsmokestatus(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmokestatus_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of current smokers
tsmokestatus_d_list_long <- tbls_pct_summary(.variable = d)

# DATABOOK prep
tsmokestatus_d <- tbls_list_split(
  .data = tsmokestatus_d_list_long, 
  .select_var = d, .vars_amount_number = 2,
  .select_var_val = "1) daily and non-daily smokers")

################################################################################

# Smoking status
tsmokestatus_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
tsmokestatus_c <- 
  tbls_list_split(.data = tsmokestatus_c_list_long, .select_var = c, .vars_amount_number = 4)

################################################################################
################################################################################

# FACTSHEET

# 1. Percentage who currently smoke tobacco

fs_tsmokestatus_d_m <- fs_summary(filter(tsmokestatus_d$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_tsmokestatus_d_w <- fs_summary(filter(tsmokestatus_d$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_tsmokestatus_d_b <- fs_summary(filter(tsmokestatus_d$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_tsmokestatus_d_joint <- cbind(fs_tsmokestatus_d_b, fs_tsmokestatus_d_m, fs_tsmokestatus_d_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who currently smoke tobacco", .before = 1)
fs_tsmokestatus_d_joint

# 2. Percentage who currently smoke tobacco daily

fs_tsmokestatus_c_m <- fs_summary(filter(tsmokestatus_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_tsmokestatus_c_w <- fs_summary(filter(tsmokestatus_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_tsmokestatus_c_b <- fs_summary(filter(tsmokestatus_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_tsmokestatus_c_joint <- cbind(fs_tsmokestatus_c_b, fs_tsmokestatus_c_m, fs_tsmokestatus_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who currently smoke tobacco daily", .before = 1)
fs_tsmokestatus_c_joint

# save the tables into CSV
library(readr)
write_excel_csv(fs_tsmokestatus_d_joint, here("FactSheet", "01_fs_tsmokestatus_d.csv"))
write_excel_csv(fs_tsmokestatus_c_joint, here("FactSheet", "02_fs_tsmokestatus_c.csv"))


################################################################################
################################################################################

