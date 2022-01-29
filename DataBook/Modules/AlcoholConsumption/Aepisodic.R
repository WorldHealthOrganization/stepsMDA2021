################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Six or more drinks on a single occasion"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "AlcoholConsumption", "functions", "Aepisodic.R"))

aepisodic_df <- aepisodic(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- aepisodic_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Six or more drinks on a single occasion at least once during the past 30 days 
# among total population
aepisodic_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
aepisodic_c <- tbls_list_split(
  .data = aepisodic_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "1) drank 6+ drinks at least once")

################################################################################

# Mean number of times with six or more drinks during a single occasion 
# in the past 30 days among current drinkers
aepisodic_a9_list_long <- tbls_mn_summary(.variable = a9, .cln = clndrinker)

# DATABOOK prep
aepisodic_a9 <- tbls_list_split(.data = aepisodic_a9_list_long, .vars_amount_number = 0)

################################################################################
################################################################################

# FACTSHEET

# 9. Percentage who engage in heavy episodic drinking (6 or more drinks on any occasion in the past 30 days)
fs_9_aepisodic_c_m <- fs_summary(filter(aepisodic_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_9_aepisodic_c_w <- fs_summary(filter(aepisodic_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_9_aepisodic_c_b <- fs_summary(filter(aepisodic_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_9_aepisodic_c_joint <- cbind(fs_9_aepisodic_c_b,
                                fs_9_aepisodic_c_m,
                                fs_9_aepisodic_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage who engage in heavy episodic drinking (6 or more drinks on any occasion in the past 30 days)", .before = 1)
fs_9_aepisodic_c_joint

library(readr)
write_excel_csv(fs_9_aepisodic_c_joint, here("FactSheet", "09_fs_aepisodic_c.csv"))


################################################################################
################################################################################

