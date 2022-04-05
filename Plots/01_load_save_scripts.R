################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# This script creates an Rds data file for making forest plots.
# Load individual R scripts from modules for mapping the forest plot function.

# load packages
library(here)
library(tidyverse)
library(fs) # for using dir_ls function
library(purrr)
library(magrittr)

################################################################################

# alcohol_data_dir 
here("DataBook", "Modules", "AlcoholConsumption") %>%
  # list all files in the directory
  dir_ls(regexp = "\\.R$") %>% 
  # scale up to all R files with a map function from purrr
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# biochemical_measurements_data_dir 
here("DataBook", "Modules", "BiochemicalMeasurements") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# cvd_data_dir 
here("DataBook", "Modules", "CardiovascularDiseaseRisk") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# cervical_cancer_data_dir 
here("DataBook", "Modules", "CervicalCancerScreening") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# demog_info_data_dir 
here("DataBook", "Modules", "DemographicInformation") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# whs_depr_data_dir 
here("DataBook", "Modules", "DepressionModuleWHS") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# diet_data_dir 
# NOT FULLY USED IN MDA (SEE CODE BELOW FOR DIET SCRIPTS)
# here("DataBook", "Modules", "Diet") %>% 
#   dir_ls(regexp = "\\.R$") %>% 
#   map(source, encoding = "UTF-8", local = knitr::knit_global())

# read in just each R file, because in MDA not scripts are used
source(here("DataBook", "Modules", "Diet", "DX1.R"))
source(here("DataBook", "Modules", "Diet", "Ddays.R"))
source(here("DataBook", "Modules", "Diet", "Dservings.R"))
source(here("DataBook", "Modules", "Diet", "Dfiveormore.R"))

# prems_data_dir 
# NOT USED IN MDA
# here("DataBook", "Modules", "HealthServicesPREMS") %>% 
#   dir_ls(regexp = "\\.R$") %>% 
#   map(source, encoding = "UTF-8", local = knitr::knit_global())

# cvd_data_dir 
here("DataBook", "Modules", "HistoryofCardiovascularDiseases") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# diabetes_data_dir 
here("DataBook", "Modules", "HistoryofDiabetes") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# bp_data_dir 
here("DataBook", "Modules", "HistoryofRaisedBloodPressure") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# cholesterol_data_dir 
here("DataBook", "Modules", "HistoryofRaisedTotalCholesterol") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# lifestyle_data_dir 
here("DataBook", "Modules", "LifestyleAdvice") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# oral_health_data_dir 
here("DataBook", "Modules", "OralHealth") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# pa_data_dir 
here("DataBook", "Modules", "PhysicalActivity") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# physical_measurements_data_dir 
here("DataBook", "Modules", "PhysicalMeasurements") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# combined_risk_factors_data_dir 
here("DataBook", "Modules", "SummaryofCombinedRiskFactors") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

# tobacco_use_data_dir 
here("DataBook", "Modules", "TobaccoUse") %>% 
  dir_ls(regexp = "\\.R$") %>% 
  map(source, encoding = "UTF-8", local = knitr::knit_global())

################################################################################

# pull "list_long" objects from the global environment 
names_all <- ls(pattern = "_list_long")

# set names (if needed), typically leave it commented out
# names_all <- purrr::set_names(names_all)

# create a list of all lists... 
# ^ to match the start of the string.
# $ to match the end of the string.
# https://r4ds.had.co.nz/strings.html#anchors
# excludes _list_long_type lists, otherwise use the 2nd option without $
list_all_list_long <- mget(ls(pattern = "_list_long$"), envir = .GlobalEnv)
# list_all_list_long <- mget(ls(pattern = "_list_long"), envir = .GlobalEnv)

# remove one extra list from the list
list_all_list_long <- list_all_list_long %>% purrr::list_modify("list_all_list_long" = NULL)

# View(list_all_list_long)


# change all a, b, c, d, e, etc. columns to a common "var" column name
# for a deep nested list
list_all_list_long_var <- lapply(list_all_list_long, function(x) 
  lapply(x, plyr::rename, c("a"="var","b"="var","c"="var","d"="var",
                            "e"="var","f"="var","g"="var",
                            "h"="var","i"="var","cd"="var",
                            "dd"="var","ed"="var","fd"="var",
                            "gd"="var","hd"="var","id"="var",
                            "diagn"="var","medstext"="var","bp_control_old"="var",
                            "htn_control"="var","rec"="var",
                            "trans"="var","work"="var","mets1200"="var",
                            "raisedbp_140_90"="var","raisedbp_140_90_or_meds"="var",
                            "raisedbp_160_100"="var","raisedbp_160_100_or_meds"="var",
                            "raisedrisk"="var",
                            # new variable added for a complete Oral Health module in MDA
                            "j"="var"))) %>% 
  # add list names as a column to each of the lists as a form of id
  imap(~ modify_depth(.x, 1, mutate, list_name=.y))

# another possible way to add list name as a new column 
# (in case if imap breaks in the future), using cbind
# imap(~ modify_depth(.x, 1, cbind, list_name=.y))

# View(list_all_list_long_var)


tidy_df_all <- 
  # flatten as a df for plotting (preserves m_w_b, m_w_b_u_r, b_reg)
  flatten_dfr(list_all_list_long_var) %>% 
  # remove pattern "_list_long" from tbl titles, mutating a new column "short_name"
  mutate(short_name = str_remove(list_name, "_list_long"))

View(tidy_df_all)

# export to a Rds file for loading later in the second script 02_make_plots.R
saveRDS(tidy_df_all, here("Plots", "tidy_df_all.rds"))

################################################################################


