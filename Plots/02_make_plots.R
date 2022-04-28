################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# This script creates forest plots in vector graphics (PDF, SVG, EMF)

# load packages
library(here)
library(tidyverse)
library(svglite)

################################################################################

# load the Rds, containing all previously saved lists
tidy_df_all <- readRDS(here("Plots", "tidy_df_all.rds"))

################################################################################

# load the mapping spreadsheet
data_book_mapping <- 
  readxl::read_excel(here("Plots", "MDA_DataBook_mapping.xlsx"))

# View(data_book_mapping)

################################################################################

# FOREST PLOTS FUNCTION
# This function creates forest plots in vector graphics (PDF, SVG, EMF) and 
# saves them into specific folders
# 
# Arguments description:
# .multi_vals - TRUE/FALSE - if one or more values should be in the output, 
# depending on the answer choices (set to FALSE by default)
# .pct_mn_md_val - "pct"/"mn"/"md" - specify the value for the output
# .ylab - "%"/"Mean"/"Median" - specify the labels on the plot
# .agerange - agerange/agerange2/region - specify the value for the legend 
# .folder - "Age_range_Sex"/"Urban_Rural"/"Region" - specify the output folder
# .ur - TRUE/FALSE - if urban/rural disaggregation should be used (set to FALSE by default)
# .save_plots - pdfs/svgs/emfs/empty for all three - save plots into vector 
# formats (set to generate all by default)

# NOTE: you can use ind <- "tsmokestatus_c" and/or ind <- "tsmokestatus_d" for testing 
# the for loop on a single variable (check comments inside the function)

forestplot_steps_m_w_b_ur_reg <- function(.multi_vals = FALSE, .pct_mn_md_val, 
                                          .ylab, .agerange, .folder, .ur = FALSE,
                                          .save_plots) {
  
  # 1 - SINGLE VALUE OUTPUT
  ##############################################################################
  # for variables with only one value to present in the output
  if(.multi_vals == FALSE) {
    
    # adjust what data to include based on the mapping spreadsheet
    # "pct_mn_md" var: percentage, mean, or median (pct/mn/md) 
    # "vals_number" var: number of output values in indicators (i.e. answer choices)
    df_mapping <- data_book_mapping %>% 
      # for percentages or means, or median and one value - "pct", "mn", "md"
      filter(pct_mn_md == {{ .pct_mn_md_val }}, vals_number==1)
    
    
    # which indicators to include by their list/script titles, using 
    # the column "include_in_analysis" (TRUE/FALSE)
    inds <- subset(df_mapping, include_in_analysis)
    
    # pull names of the selected lists/scripts into a vector
    reqnames <- inds$tbls_short_name
    
    # scripts to include that match
    scripts_inc <- intersect(inds$tbls_short_name, tidy_df_all$short_name)
    
    # filter data by column 26 ("short_name") to match the vector "reqnames"
    data_inc <- tidy_df_all %>% filter(short_name %in% reqnames)
    
    # list of values/answer choices that should be excluded from plotting
    # NOTE: the var "vals_exclude" in the mapping spreadsheet contains multiple 
    # answer choices separated by ";" in a string
    vals_exclude_cln <- strsplit(inds$vals_exclude, "[;]") %>% 
      lapply(., function(x) x[!is.na(x)]) %>% as_vector()
    
    # plot with a for loop
    for (ind in intersect(inds$tbls_short_name, unique(data_inc$short_name))) {
      
      # single indicator for testing the for loop
      # ind <- "tsmokestatus_d"
      # ind <- "cvdrisk_c"
      # ind <- "pcomposition_work"
      
      # prepare data for plotting
      plotvalues <- 
        # NOTE: specific indicators require additional conditions and variable preparations,
        # as they have their own variables/columns 
        filter(
          .data = if(ind=="cvdrisk_c" | ind=="cvdrisk_d"){ 
            mutate(tidy_df_all,
                   # create a new agerange var from agerangecvd var, applying the same factor levels
                   # this is for regional plots
                   agerange = factor(ifelse(is.na(ur) & is.na(region), as.character(agerangecvd), agerange)), 
                   # the same applies to agerange2 var for urban/rural plots
                   agerange2 = factor(ifelse(!is.na(ur), as.character(agerangecvd), agerange2))) } 
          else tidy_df_all,
          short_name==ind) %>% 
        drop_na({{ .agerange }}) %>%
        # include only values that don't match vals_exclude
        filter(!var %in% vals_exclude_cln)
      
      
      tryCatch(
        {
          p <- ggplot(data = plotvalues, 
                      aes(x = {{ .agerange }}, 
                          y = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m else md, 
                          ymin = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m_low else md_low, 
                          ymax = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m_upp else md_upp, 
                          color = {{ .agerange }}, shape = if(.ur) ur)) +
            geom_pointrange(position = position_dodge(width = 1), size = 0.15) +
            ggforce::facet_col(facets = ~sex, scales = "free_y", space = "free") + # group plots by sex
            coord_flip() +  # flip coordinates (puts labels on y axis)
            labs(title = stringr::str_wrap(paste(inds$tbl_title[inds$tbls_short_name==ind]), 60)) + # add main title
            xlab(NULL) + # remove x label 
            ylab({{ .ylab }}) + # specify y label
            labs(color = if(deparse(substitute(.agerange))=="agerange" | 
                            deparse(substitute(.agerange))=="agerange2") "Age range" else "Region", 
                 shape = if(.ur) "Settlement") + # adjust label text for agerange and ur. NOTE: {{ }} syntax doesn't work here
            theme_bw() + # use a white background
            theme(text = element_text(size = 8), # set font size
                  axis.text.y = element_blank(),
                  axis.ticks.y = element_blank(),
                  panel.grid.major = element_blank(), 
                  panel.grid.minor = element_blank(), # remove xlab for cases of vals_number == 1
                  plot.title = element_text(size = 8)) + # set title's font size
            geom_hline(yintercept = 0, linetype = 2, color = "grey") + # set horizontal lines for zero
            ggsci::scale_color_lancet() # color pattern for geom points in ggplot
          
          print(p)
          
          # save into three common vectorized formats (for editing plots after saving)
          if (deparse(substitute(.save_plots))=="pdfs"){
            
            # save into PDF
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".pdf"), device = cairo_pdf, 
                   path = here("Plots", "PDFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else if (deparse(substitute(.save_plots))=="svgs"){
            
            # save into SVG
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".svg"), device = "svg",
                   path = here("Plots", "SVGs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else if (deparse(substitute(.save_plots))=="emfs"){
            
            # save into EMF
            # NOTE: EMF format requires devEMF package
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".emf"),
                   device = {function(filename, ...) devEMF::emf(file = filename, ...)},
                   path = here("Plots", "EMFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else {
            
            # all three formats at the same time
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".pdf"), device = cairo_pdf, 
                   path = here("Plots", "PDFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".svg"), device = "svg",
                   path = here("Plots", "SVGs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".emf"),
                   device = {function(filename, ...) devEMF::emf(file = filename, ...)},
                   path = here("Plots", "EMFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          }
          
        }, error = function(cond) {
          
          message(paste("Could not make a plot for indicator", ind))
          return(NA)
          
        }) 
      
    }
    
    
  } 
  # 2 - MULTIPLE VALUES OUTPUT
  ##############################################################################
  # for variables with several values to present in the output
  else {
    
    # adjust what data to include based on the mapping spreadsheet
    # "pct_mn_md" var: percentage, mean, or median (pct/mn/md) 
    # "vals_number" var: number of output values in indicators (i.e. answer choices)
    df_mapping <- data_book_mapping %>% 
      # for percentages and more than one value
      filter(pct_mn_md == {{ .pct_mn_md_val }}, vals_number>1)
    
    # which indicators to include by their list/script titles, using 
    # the column "include_in_analysis" (TRUE/FALSE)
    inds <- subset(df_mapping, include_in_analysis)
    
    # pull names of the selected lists/scripts into a vector
    reqnames <- inds$tbls_short_name
    
    # scripts to include that match
    scripts_inc <- intersect(inds$tbls_short_name, tidy_df_all$short_name)
    
    # filter data by column 26 ("short_name") to match the vector "reqnames"
    data_inc <- tidy_df_all %>% filter(short_name %in% reqnames)
    
    # list of values/answer choices that should be excluded from plotting
    # NOTE: the var "vals_exclude" in the mapping spreadsheet contains multiple 
    # answer choices separated by ";" in a string
    vals_exclude_cln <- strsplit(inds$vals_exclude, "[;]") %>% 
      lapply(., function(x) x[!is.na(x)]) %>% as_vector()
    
    
    # plot a for loop
    for (ind in intersect(inds$tbls_short_name, unique(data_inc$short_name))) {
      
      # single indicator for multiple answer choices testing
      # ind <- "tsmokestatus_c"
      # ind <- "raisedrisk"
      
      plotvalues <- 
        # NOTE: specific indicators require additional conditions and variable preparations,
        # as they have their own variables/columns 
        filter(
          .data = if(ind=="raisedrisk"){ 
            mutate(tidy_df_all, 
                   # create a new agerange var from agerangecvd var, applying the same factor levels
                   # this is for regional plots
                   agerange = factor(ifelse(is.na(ur) & is.na(region), as.character(agerangerr), agerange)),
                   # the same applies to agerange2 var for urban/rural plots
                   agerange2 = factor(ifelse(!is.na(ur), as.character(agerangerr), agerange2))) }
          else tidy_df_all,
          short_name==ind) %>% 
        # arrange vars on the lab (NOTE: needed for the multiple answer choices case)
        mutate(var = fct_reorder(var, desc(var))) %>% 
        drop_na({{ .agerange }}) %>% 
        # include only values that don't match vals_exclude
        filter(!var %in% vals_exclude_cln)
      
      
      tryCatch(
        {
          p <- ggplot(data=plotvalues, 
                      aes(x = var, 
                          y = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m else md, 
                          ymin = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m_low else md_low, 
                          ymax = if(.pct_mn_md_val=="pct" | .pct_mn_md_val=="mn") m_upp else md_upp, 
                          color = {{ .agerange }}, shape = if(.ur) ur)) +
            geom_pointrange(position = position_dodge(width = 1), size = 0.15) +
            ggforce::facet_col(facets = ~sex, scales = "free_y", space = "free") + # group plots by sex
            coord_flip() +  # flip coordinates (puts labels on y axis)
            labs(title = stringr::str_wrap(paste(inds$tbl_title[inds$tbls_short_name==ind]), 60)) + # add main title
            xlab(NULL) + # remove x label 
            ylab({{ .ylab }}) + # specify y label
            labs(color = if(deparse(substitute(.agerange))=="agerange" | 
                            deparse(substitute(.agerange))=="agerange2") "Age range" else "Region", 
                 shape = if(.ur) "Settlement") + # adjust label text for agerange and ur
            theme_bw() + # use a white background
            theme(text = element_text(size = 8), # set font size
                  panel.grid = element_blank(), # remove grid lines
                  plot.title = element_text(size = 8)) + # set title's font size
            geom_vline(xintercept = seq(0.5, length(plotvalues$var), by = 1), 
                       color = "gray", size = 0.5, alpha = 0.5) + # set vertical lines between x groups
            geom_hline(yintercept = 0, linetype = 2, color = "grey") + # set horizontal lines for zero
            ggsci::scale_color_lancet() # color pattern for geom points in ggplot
          
          print(p)
          
          # save into three common vectorized formats (for editing plots after saving)
          if (deparse(substitute(.save_plots))=="pdfs"){
            
            # save into PDF
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".pdf"), device = cairo_pdf, 
                     path = here("Plots", "PDFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else if (deparse(substitute(.save_plots))=="svgs"){
            
            # save into SVG
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".svg"), device = "svg",
                   path = here("Plots", "SVGs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else if (deparse(substitute(.save_plots))=="emfs"){
            
            # save into EMF
            # NOTE: EMF format requires devEMF package
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".emf"),
                   device = {function(filename, ...) devEMF::emf(file = filename, ...)},
                   path = here("Plots", "EMFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          } else {
            
            # all three formats at the same time
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".pdf"), device = cairo_pdf, 
                   path = here("Plots", "PDFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".svg"), device = "svg",
                   path = here("Plots", "SVGs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            ggsave(plot = p, filename = paste0(ind, if(.folder=="Region"){ paste0("_reg") } else if(.folder=="Urban_Rural"){ paste0("_u_r") }, ".emf"),
                   device = {function(filename, ...) devEMF::emf(file = filename, ...)},
                   path = here("Plots", "EMFs", if(!missing(.folder)) .folder ), width = 6, height = 6)
            
          }
          
        }, error = function(cond) {
          
          message(paste("Could not make a plot for indicator", ind))
          return(NA)
          
        }) 
      
    }
    
    
  }
  
  #beepr::beep() # make a sound notification upon completion
  message("Completed!")
  
}

################################################################################
# EXAMPLES OF USAGE
################################################################################

# PERCENTAGES

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps_m_w_b_ur_reg(.multi_vals = TRUE, .pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = agerange, .folder = "Age_range_Sex")
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = agerange, .folder = "Age_range_Sex")


# URBAN, RURAL
forestplot_steps_m_w_b_ur_reg(.multi_vals = TRUE, .pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps_m_w_b_ur_reg(.multi_vals = TRUE, .pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = region, .folder = "Region")
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "pct", .ylab = "%", 
                              .agerange = region, .folder = "Region")


################################################################################

# MEANS

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "mn", .ylab = "Mean", 
                              .agerange = agerange, .folder = "Age_range_Sex")


# URBAN, RURAL
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "mn", .ylab = "Mean", 
                              .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "mn", .ylab = "Mean", 
                              .agerange = region, .folder = "Region")

################################################################################

# MEDIANS

# MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "md", .ylab = "Median", 
                              .agerange = agerange, .folder = "Age_range_Sex")


# URBAN, RURAL
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "md", .ylab = "Median", 
                              .agerange = agerange2, .folder = "Urban_Rural", .ur = TRUE)

# REGION
forestplot_steps_m_w_b_ur_reg(.pct_mn_md_val = "md", .ylab = "Median", 
                              .agerange = region, .folder = "Region")


################################################################################


