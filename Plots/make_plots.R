################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# load packages
library(here)
library(tidyverse)
library(fs) # for using dir_ls function
library(magrittr)

################################################################################
# PART 1 - load individual R scripts from modules for mapping the forest plot 
# function and create an Rds data file for making forest plots
################################################################################

# load all scripts into global environment by using a function
run_all_scripts <- function() {
  
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
  
  # read in each R file, because in MDA not all scripts are used
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
  
}

# NOTE: the function below will require some time to run
# For reference, on a MacBook with the M1 processor and 8Gb RAM, it runs for 10 mins
run_all_scripts()

################################################################################

# create a list of all lists... 
# ^ to match the start of the string.
# $ to match the end of the string.
# https://r4ds.had.co.nz/strings.html#anchors
# excludes _list_long_type lists, otherwise use the 2nd option without $
list_all_list_long <- mget(ls(pattern = "_list_long$"), envir = .GlobalEnv)
# list_all_list_long <- mget(ls(pattern = "_list_long"), envir = .GlobalEnv)

# remove one extra list from the list (in case mget from above was run twice)
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

# save to a Rds file for reference and loading later if needed
saveRDS(tidy_df_all, here("Plots", "tidy_df_all.rds"))

################################################################################

# if the above part of script was run before, 
# load the Rds, containing all previously saved lists
tidy_df_all <- readRDS(here("Plots", "tidy_df_all.rds"))

################################################################################

# load the mapping spreadsheet
data_book_mapping <- 
  readxl::read_excel(here("Plots", "MDA_DataBook_mapping.xlsx"))

# View(data_book_mapping)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################
# PART 2 - create forest plots in vector graphics (PDF, SVG, EMF)
# EXAMPLES OF USAGE
################################################################################

# PERCENTAGES

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps(.multi_vals = TRUE, .pct_mn_md_val = pct, .ylab = "%", 
                 .agerange = agerange, .folder = "Age_range_Sex")
forestplot_steps(.pct_mn_md_val = pct, .ylab = "%",
                 .agerange = agerange, .folder = "Age_range_Sex")

# URBAN, RURAL
forestplot_steps(.multi_vals = TRUE, .pct_mn_md_val = pct, .ylab = "%", 
                 .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)
forestplot_steps(.pct_mn_md_val = pct, .ylab = "%", 
                 .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps(.multi_vals = TRUE, .pct_mn_md_val = pct, .ylab = "%",
                 .agerange = region, .folder = "Region")
forestplot_steps(.pct_mn_md_val = pct, .ylab = "%",
                 .agerange = region, .folder = "Region")


################################################################################

# MEANS

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps(.pct_mn_md_val = mn, .ylab = "Mean", 
                 .agerange = agerange, .folder = "Age_range_Sex")

# URBAN, RURAL
forestplot_steps(.pct_mn_md_val = mn, .ylab = "Mean", 
                 .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps(.pct_mn_md_val = mn, .ylab = "Mean", 
                 .agerange = region, .folder = "Region")

################################################################################

# MEDIANS

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps(.pct_mn_md_val = md, .ylab = "Median", 
                 .agerange = agerange, .folder = "Age_range_Sex")

# URBAN, RURAL
forestplot_steps(.pct_mn_md_val = md, .ylab = "Median", 
                 .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps(.pct_mn_md_val = md, .ylab = "Median",
                 .agerange = region, .folder = "Region")


################################################################################



