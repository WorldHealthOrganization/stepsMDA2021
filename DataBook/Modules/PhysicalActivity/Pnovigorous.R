################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "No vigorous physical activity"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

# Load Clean Recode P1-P15 script

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "CleanRecodeP1-P15.R"))

data <- cleanrecodep1p15(data)

################################################################################

# Load the function for this indicator

source(here("DataBook", "Modules", "PhysicalActivity", "functions", "Pnovigorous.R"))

pnovigorous_df <- pnovigorous(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- pnovigorous_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# No vigorous physical activity
pnovigorous_c_list_long <- tbls_pct_summary(.variable = c)

# DATABOOK prep
pnovigorous_c <- tbls_list_split(
  .data = pnovigorous_c_list_long, 
  .select_var = c, .vars_amount_number = 2,
  .select_var_val = "did no vigorous physical activity")

################################################################################

# FACTSHEET

# 19. Percentage not engaging in vigorous activity
fs_19_pnovigorous_c_m <- fs_summary(filter(pnovigorous_c$m, agerange == "18–69"), c(3,4,5), Males, .pct)
fs_19_pnovigorous_c_w <- fs_summary(filter(pnovigorous_c$w, agerange == "18–69"), c(3,4,5), Females, .pct)
fs_19_pnovigorous_c_b <- fs_summary(filter(pnovigorous_c$b, agerange == "18–69"), c(3,4,5), "Both sexes", .pct)

fs_19_pnovigorous_c_joint <- cbind(fs_19_pnovigorous_c_b,
                                   fs_19_pnovigorous_c_m,
                                   fs_19_pnovigorous_c_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage not engaging in vigorous activity", .before = 1)
fs_19_pnovigorous_c_joint

readr::write_excel_csv(fs_19_pnovigorous_c_joint, here("FactSheet", "19_fs_pnovigorous_c.csv"))


################################################################################




