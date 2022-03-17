################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# This script creates forest plots in vector graphics (PDF, SVG, EMF)

# load packages
library(here)
library(tidyverse)
library(fs)
library(ggforestplot) # this requires m_se variable in the dataframe and m as numbers
library(ggforce)
library(purrr)
library(readxl)

################################################################################

# load the Rds, containing all previously saved lists with 01_load_save_scripts.R
tidy_df_all <- readRDS(here("Plots", "tidy_df_all.rds"))

################################################################################

# load the mapping spreadsheet, where "include_in_analysis" var can be adjusted
# from TRUE to FALSE for including or excluding in plotting
DataBook_mapping <- read_excel(here("Plots", "MDA_DataBook_mapping.xlsx"))

# View(DataBook_mapping)

################################################################################

# adjust what data to include based on the mapping spreadsheet
# NOTE: below is a chunk of code that is manually adjusted for including/excluding 
# specific parts from the mapping spreadsheet
# "pct_mn_md" var: percentage, mean, or median (pct/mn/md) 
# "vals_number" var: number of output values in indicators (i.e. answer choices)
df_mapping <- DataBook_mapping %>% 
  # for percentages and one value
  # filter(pct_mn_md=="pct", vals_number==1) # %>%
  # for means and one value
  # filter(pct_mn_md=="mn", vals_number==1) # %>%
  # for percentages and more than one value
  filter(pct_mn_md=="pct", vals_number>1)

# which indicators to include by their list/script titles, using 
# the column "include_in_analysis" (TRUE/FALSE)
inds <- subset(df_mapping, include_in_analysis)

# pull names of the selected lists/scripts into a vector
reqnames <- inds$tbls_short_name

# scripts to include that match
scripts_inc <- intersect(inds$tbls_short_name, tidy_df_all$short_name)

# filter data by column 26 - "short_name" to match the vector "reqnames"
data_inc <- tidy_df_all %>% filter(!!sym(names(.)[26]) %in% reqnames) 

# View(data_inc)

# list of values/answer choices that should be excluded from plotting
# NOTE: the var "vals_exclude" in the mapping spreadsheet contains multiple 
# answer choices separated by ";" in a string
vals_exclude_cln <- strsplit(inds$vals_exclude, "[;]") %>% 
  lapply(., function(x) x[!is.na(x)])

################################################################################

# option to save PDFs, etc. (TRUE/FALSE)
generatePDF <- TRUE

################################################################################

# MEN, WOMEN, BOTH SEXES
for (ind in intersect(inds$tbls_short_name, unique(data_inc$short_name))) {
  
  # prepare data for plotting
  plotvalues <- subset(tidy_df_all, short_name==ind) %>% 
    mutate(agerange = fct_reorder(agerange, desc(agerange))) %>% 
    drop_na(agerange) %>% 
    # include only values that don't match vals_exclude
    filter(!var %in% vals_exclude_cln)
  
  tryCatch(
    {
      p <- forestplot(
        df = plotvalues, name = var, estimate = m, se = m_se,
        # added str_wrap function to wrap title text at 60 characters if too long
        title = stringr::str_wrap(paste(inds$tbl_title[inds$tbls_short_name==ind]), 60),
        colour = agerange,
        # NOTE: change "%" to "Mean" for cases when pct_mn_md=="mn"
        xlab = "%") + 
        labs(colour = "Age range") + 
        theme(text = element_text(family = "ArialMT", size = 9),
              plot.title = element_text(size = 8),
              # remove ylab for cases of vals_number == 1 
              # NOTE: check code above for df_mapping
              # axis.text.y = element_blank()
              ) +
        # group plots by sex (with a function from ggforce)
        facet_col(facets = ~sex, scales = "free_y", space = "free")
      
      print(p)
      
      if (generatePDF){
        
        # save into three common vectorized formats (for editing plots after saving)
        ggsave(plot = p, filename = paste0(ind,".pdf"), device = cairo_pdf, 
               path = here("Plots", "PDFs"), scale = 1.25)
        ggsave(plot = p, filename = paste0(ind,".svg"), device = "svg", 
               path = here("Plots", "SVGs"), scale = 1.25)
        # EMF format requires devEMF package
        ggsave(plot = p, filename = paste0(ind,".emf"), 
               device = {function(filename, ...) devEMF::emf(file = filename, ...)}, 
               path = here("Plots", "EMFs"), scale = 1.25)
        
      }
      
    }, error = function(cond) {
      
      message(paste("Could not make a plot for indicator", ind))
      return(NA)
      
      }) 
  
}

################################################################################

# URBAN, RURAL - MEN, WOMEN, BOTH SEXES
for (ind in intersect(inds$tbls_short_name, unique(data_inc$short_name))) {

  # prepare data for plotting
  plotvalues <- subset(tidy_df_all, short_name==ind) %>% 
    mutate(agerange2 = fct_reorder(agerange2, desc(agerange2))) %>% 
    drop_na(agerange2) %>% 
    # include only values that don't match vals_exclude
    filter(!var %in% vals_exclude_cln)
  
  tryCatch(
    {
      p <- forestplot(
        df = plotvalues, name = var, estimate = m, se = m_se,
        title = stringr::str_wrap(paste(inds$tbl_title[inds$tbls_short_name==ind]), 60),
        colour = agerange2, 
        # NOTE: change "%" to "Mean" for cases when pct_mn_md=="mn"
        xlab = "%", shape = ur) + 
        labs(colour = "Age range", shape = "Settlement") + 
        theme(text = element_text(family = "ArialMT", size = 9),
              plot.title = element_text(size = 8), 
              # remove ylab for cases of vals_number == 1 
              # axis.text.y = element_blank()
              ) +
        # group plots by sex (with a function from ggforce)
        facet_col(facets = ~sex, scales = "free_y", space = "free")
      
      print(p)
      
      if (generatePDF){
        
        # save into three common vectorized formats (for editing plots after saving)
        ggsave(plot = p, filename = paste0(ind,"_u_r",".pdf"), device = cairo_pdf, 
               path = here("Plots", "PDFs", "Urban_Rural"), scale = 1.25)
        ggsave(plot = p, filename = paste0(ind,"_u_r",".svg"), device = "svg", 
               path = here("Plots", "SVGs", "Urban_Rural"), scale = 1.25)
        # EMF format requires devEMF package
        ggsave(plot = p, filename = paste0(ind,"_u_r",".emf"), 
               device = {function(filename, ...) devEMF::emf(file = filename, ...)}, 
               path = here("Plots", "EMFs", "Urban_Rural"), scale = 1.25)
        
      }
      
    }, error = function(cond) {
      
      message(paste("Could not make a plot for indicator", ind))
      return(NA)
      
      }) 
  
}

################################################################################

# REGION - BOTH SEXES
for (ind in intersect(inds$tbls_short_name, unique(data_inc$short_name))) {
  
  # prepare data for plotting
  plotvalues <- subset(tidy_df_all, short_name==ind) %>% 
    mutate(region = fct_reorder(region, desc(region))) %>% 
    drop_na(region) %>% 
    # include only values that don't match vals_exclude
    filter(!var %in% vals_exclude_cln)
  
  tryCatch(
    {
      p <- forestplot(
        df = plotvalues, name = var, estimate = m, se = m_se,
        title = stringr::str_wrap(paste(inds$tbl_title[inds$tbls_short_name==ind]), 60),
        colour = region, 
        # NOTE: change "%" to "Mean" for cases when pct_mn_md=="mn"
        xlab = "%") + 
        labs(colour = "Region") + 
        theme(text = element_text(family = "ArialMT", size = 9),
              plot.title = element_text(size = 8),
              # remove ylab for cases of vals_number == 1 
              # axis.text.y = element_blank()
              ) +
        # group plots by sex (with a function from ggforce)
        facet_col(facets = ~sex, scales = "free_y", space = "free")
      
      print(p)
      
      if (generatePDF){
        
        # save into three common vectorized formats (for editing plots after saving)
        ggsave(plot = p, filename = paste0(ind,"_reg",".pdf"), device = cairo_pdf, 
               path = here("Plots", "PDFs", "Region"), scale = 1.25)
        ggsave(plot = p, filename = paste0(ind,"_reg",".svg"), device = "svg", 
               path = here("Plots", "SVGs", "Region"), scale = 1.25)
        # EMF format requires devEMF package
        ggsave(plot = p, filename = paste0(ind,"_reg",".emf"), 
               device = {function(filename, ...) devEMF::emf(file = filename, ...)}, 
               path = here("Plots", "EMFs", "Region"), scale = 1.25)
        
      }
      
    }, error = function(cond) {
      
      message(paste("Could not make a plot for indicator", ind))
      return(NA)
      
      }) 
  
}


################################################################################


