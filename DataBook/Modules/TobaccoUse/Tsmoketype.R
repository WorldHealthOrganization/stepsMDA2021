################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Amount of tobacco used by type among current daily smokers"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################
# Load the function for this indicator

source(here("DataBook", "Modules", "TobaccoUse", "functions", "Tsmoketype.R"))

tsmoketype_df <- tsmoketype(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- tsmoketype_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Mean amount of tobacco used by daily smokers by type

# Men, women, both sexes, urban, rural, region (both sexes)

# 1 - Mean # of manufactured cig.
tsmoketype_t5a_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5a, .cln2 = t5acln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5a <- tbls_list_split(.data = tsmoketype_t5a_list_long, .vars_amount_number = 0)

# 2 - Mean # of hand-rolled cig.
tsmoketype_t5b_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5b, .cln2 = t5bcln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5b <- tbls_list_split(.data = tsmoketype_t5b_list_long, .vars_amount_number = 0)

# 3 - Mean # of pipes of tobacco
tsmoketype_t5c_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5c, .cln2 = t5ccln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5c <- tbls_list_split(.data = tsmoketype_t5c_list_long, .vars_amount_number = 0)

# 4 - Mean # of cigars, cheerots, cigarillos
tsmoketype_t5d_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5d, .cln2 = t5dcln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5d <- tbls_list_split(.data = tsmoketype_t5d_list_long, .vars_amount_number = 0)

# 5 - Mean # of shisha sessions
tsmoketype_t5e_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5e, .cln2 = t5ecln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5e <- tbls_list_split(.data = tsmoketype_t5e_list_long, .vars_amount_number = 0)

# 6 - Mean # of heated tobacco products
tsmoketype_t5f_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5f, .cln2 = t5fcln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5f <- tbls_list_split(.data = tsmoketype_t5f_list_long, .vars_amount_number = 0)

# 7 - Mean # of other type of tobacco
tsmoketype_t5g_list_long <- tbls_summary(.mn_pct_md = mn, .variable = t5g, .cln2 = t5gcln, .cln2_val = 1)
# DATABOOK prep
tsmoketype_t5g <- tbls_list_split(.data = tsmoketype_t5g_list_long, .vars_amount_number = 0)

################################################################################

# Adding tobacco type column for joining the above into one single long list

tsmoketype_t5a_list_long_type <- tsmoketype_t5a_list_long %>% map(~ mutate(., type = factor("Manufactured cigs")))
tsmoketype_t5b_list_long_type <- tsmoketype_t5b_list_long %>% map(~ mutate(., type = factor("Hand-rolled cigs")))
tsmoketype_t5c_list_long_type <- tsmoketype_t5c_list_long %>% map(~ mutate(., type = factor("Pipes")))
tsmoketype_t5d_list_long_type <- tsmoketype_t5d_list_long %>% map(~ mutate(., type = factor("Cigars, cheerots, cigarillos")))
tsmoketype_t5e_list_long_type <- tsmoketype_t5e_list_long %>% map(~ mutate(., type = factor("Shisha")))
tsmoketype_t5f_list_long_type <- tsmoketype_t5f_list_long %>% map(~ mutate(., type = factor("Heated tobacco products")))
tsmoketype_t5g_list_long_type <- tsmoketype_t5g_list_long %>% map(~ mutate(., type = factor("Other type of tobacco")))


f <- function(.x, .y) {
  if(is.list(.x)) purrr::map2(.x, .y, f) else c(.x, .y)
}

t5a_b <- f(tsmoketype_t5a_list_long_type, tsmoketype_t5b_list_long_type)
t5a_b_c <- f(t5a_b, tsmoketype_t5c_list_long_type)
t5a_b_c_d <- f(t5a_b_c, tsmoketype_t5d_list_long_type)
t5a_b_c_d_e <- f(t5a_b_c_d, tsmoketype_t5e_list_long_type)
t5a_b_c_d_e_f <- f(t5a_b_c_d_e, tsmoketype_t5f_list_long_type)
t5a_b_c_d_e_f_g <- f(t5a_b_c_d_e_f, tsmoketype_t5g_list_long_type) %>% map(~ as_tibble(.))
tsmoketype_list_long <- t5a_b_c_d_e_f_g

################################################################################

# FACTSHEET

# 5. Mean number of manufactured cigarettes smoked per day (by smokers of manufactured cigarettes)
fs_tsmoketype_t5a_daily_m <- fs_summary(filter(tsmoketype_t5a$m, agerange == "18–69"), c(3,4,5), Males)
fs_tsmoketype_t5a_daily_w <- fs_summary(filter(tsmoketype_t5a$w, agerange == "18–69"), c(3,4,5), Females)
fs_tsmoketype_t5a_daily_b <- fs_summary(filter(tsmoketype_t5a$b, agerange == "18–69"), c(3,4,5), "Both sexes")

fs_tsmoketype_t5a_daily_joint <- cbind(fs_tsmoketype_t5a_daily_b,
                                       fs_tsmoketype_t5a_daily_m,
                                       fs_tsmoketype_t5a_daily_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Mean number of manufactured cigarettes smoked per day (by smokers of manufactured cigarettes)", .before = 1)

readr::write_excel_csv(fs_tsmoketype_t5a_daily_joint, here("FactSheet", "05_fs_tsmoketype_t5a_daily.csv"))

################################################################################
################################################################################

