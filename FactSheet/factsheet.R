################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# load CSV data files for joining into one df for the fact sheet

# load packages
library(here)
library(tidyverse)
library(fs) # for using dir_ls function

fs_df <- here("FactSheet") %>% 
  # read in only CSV files
  dir_ls(regexp = "\\.csv$") %>% 
  # scale up to all CSV files with a map functions from purrr
  map_dfr(read_csv)

fs_df


# create a Word table (two versions are possible with different packages)
library(flextable)
fs_df_flex <- qflextable(fs_df) %>% width(width = 1.5)
save_as_docx("factsheet" = fs_df_flex, path = "FactSheet/fs_flextable_version.docx")

library(huxtable)
fs_df_hux <- hux(fs_df)
width(fs_df_hux) <- 1
quick_docx(fs_df_hux, file = ("FactSheet/fs_huxtable_version.docx"))




