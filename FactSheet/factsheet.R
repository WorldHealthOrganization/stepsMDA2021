################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# load CSV data files for joining into one df for the FactSheet

# load packages
library(here)
library(tidyverse)
library(fs) # for using dir_ls function

# set the directory
data_dir <- here("FactSheet")

# list all files in the directory
dir_ls(data_dir)

# only CSV files
csv_files <- dir_ls(data_dir, regexp = "\\.csv$")
csv_files

# read in just one file
# readr::read_csv(csv_files[1])

# scale up to all CSV files with a map functions from purrr
fs_df <- csv_files %>% 
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




