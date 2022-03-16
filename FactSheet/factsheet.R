################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# load CSV data files for joining into one df for the FactSheet

library(tidyverse)
library(fs)
library(here)

data_dir <- here::here("FactSheet")

# list all files in the directory
fs::dir_ls(data_dir)

# only CSV files
csv_files <- fs::dir_ls(data_dir, regexp = "\\.csv$")
csv_files

# read in just one file
# readr::read_csv(csv_files[1])

# scale up to all CSV files with a map functions from purrr
fs_df <- csv_files %>% 
  map_dfr(read_csv)

fs_df


# create a Word table
# library(officer)
# doc_table <- read_docx(path = "FactSheet/ref_doc_02.docx") %>% 
#   body_add_table(head(fs_df, n = 38),style = "TestStyle", first_column = TRUE) 
# print(doc_table, target = "FactSheet/example_table.docx")

library(flextable)
fs_df <- qflextable(fs_df) #%>% width(width = 1.5)
save_as_docx("factsheet" = fs_df, path = "FactSheet/flextable1.docx")

library(huxtable)
fs_df <- hux(fs_df)
width(fs_df) <- 1
quick_docx(fs_df, file = ("FactSheet/fs_output.docx"))




