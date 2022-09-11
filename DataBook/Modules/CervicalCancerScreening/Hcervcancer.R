################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Cancer screening"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

source(here("DataBook", "Modules", "CervicalCancerScreening", "functions", "Hcervcancer.R"))

hcervcancer_df <- hcervcancer(data)

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- hcervcancer_df %>% 
  as_survey_design(ids=psu, strata=stratum, weights=wstep1, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# Percentage of female respondents who have ever had a screening test for 
# cervical cancer among all female respondents
hcervcancer_c_w_list_long <- tbls_summary(.mn_pct_md = pct, .variable = c)

# DATABOOK prep
hcervcancer_c_w <- tbls_list_split(
  .data = hcervcancer_c_w_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) has been screened")

################################################################################

# Women aged 30-49 (use TOTAL line - "18–69", which is filtered to a specific range)
hcervcancer_c2_w_list_long <- STEPSClean %>% filter(age>=30, age<50) %>% 
  tbls_summary(., .mn_pct_md = pct, .variable = c)

# DATABOOK prep
hcervcancer_c2_w <- tbls_list_split(
  .data = hcervcancer_c2_w_list_long, .select_var = c, 
  .vars_amount_number = 2, .select_var_val = "1) has been screened")

hcervcancer_c2_w_t <- hcervcancer_c2_w$w %>% filter(agerange == "18–69")

################################################################################

# FACTSHEET

# 20. Percentage of women aged 30–49 years who have ever had a screening test for cervical cancer
fs_20_hcervcancer_c2_w <- fs_summary(hcervcancer_c2_w_t, c(3,4,5), Females, .pct)

fs_20_hcervcancer_c2_joint <- cbind(fs_20_hcervcancer_c2_w) %>%
  mutate("Results for adults aged 18–69 years (incl. 95% CI)" =
           "Percentage of women aged 30–49 years who have ever had a screening test for cervical cancer", .before = 1)

readr::write_excel_csv(fs_20_hcervcancer_c2_joint, here("FactSheet", "20_fs_hcervcancer_c2_c.csv"))


################################################################################


