################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# LIST OF FUNCTIONS IN THIS SCRIPT
# unite_ci - unite low and upper confidence intervals into one CI variable
# apply_hux - create hux tables
# final_m_w_b_tbl - output clean-looking main tables (for men, women, both sexes)
# final_u_r_tbl - output clean-looking urban/rural tables
# join_m_w_b - join individual hux tables into one (men, women, both sexes)
# join_u_r - join individual hux tables into one (urban, rural disaggregation)

################################################################################

# Load necessary packages
library(huxtable)
library(tidyr) # for using the unite function 

################################################################################

# Function to unite low and upper confidence intervals into one CI variable
# Arguments description:
# .ci_amount - value for the amount of CIs in the table (from 1 to 7, or 
# "q" is used for PA Inter-quartile range (P25-P75))

unite_ci <- function(.data, .ci_amount) {
  if(.ci_amount == 7) {
    .data %>%
      unite(ci1, m_low1, m_upp1, sep = "–", remove = TRUE) %>%
      unite(ci2, m_low2, m_upp2, sep = "–", remove = TRUE) %>%
      unite(ci3, m_low3, m_upp3, sep = "–", remove = TRUE) %>%
      unite(ci4, m_low4, m_upp4, sep = "–", remove = TRUE) %>% 
      unite(ci5, m_low5, m_upp5, sep = "–", remove = TRUE) %>%
      unite(ci6, m_low6, m_upp6, sep = "–", remove = TRUE) %>% 
      unite(ci7, m_low7, m_upp7, sep = "–", remove = TRUE)
  } 
  else if (.ci_amount == 6) {
    .data %>% 
      unite(ci1, m_low1, m_upp1, sep = "–", remove = TRUE) %>%
      unite(ci2, m_low2, m_upp2, sep = "–", remove = TRUE) %>%
      unite(ci3, m_low3, m_upp3, sep = "–", remove = TRUE) %>%
      unite(ci4, m_low4, m_upp4, sep = "–", remove = TRUE) %>% 
      unite(ci5, m_low5, m_upp5, sep = "–", remove = TRUE) %>%
      unite(ci6, m_low6, m_upp6, sep = "–", remove = TRUE) 
  }
  else if (.ci_amount == 5) {
    .data %>% 
      unite(ci1, m_low1, m_upp1, sep = "–", remove = TRUE) %>%
      unite(ci2, m_low2, m_upp2, sep = "–", remove = TRUE) %>%
      unite(ci3, m_low3, m_upp3, sep = "–", remove = TRUE) %>%
      unite(ci4, m_low4, m_upp4, sep = "–", remove = TRUE) %>% 
      unite(ci5, m_low5, m_upp5, sep = "–", remove = TRUE)
  }
  else if (.ci_amount == 4) {
    .data %>%
      unite(ci1, m_low1, m_upp1, sep = "–", remove = TRUE) %>%
      unite(ci2, m_low2, m_upp2, sep = "–", remove = TRUE) %>%
      unite(ci3, m_low3, m_upp3, sep = "–", remove = TRUE) %>%
      unite(ci4, m_low4, m_upp4, sep = "–", remove = TRUE) 
  }
  else if (.ci_amount == 3) {
    .data %>%  
      unite(ci1, m_low1, m_upp1, sep = "–", remove = TRUE) %>%
      unite(ci2, m_low2, m_upp2, sep = "–", remove = TRUE) %>%
      unite(ci3, m_low3, m_upp3, sep = "–", remove = TRUE) 
  }
  else if (.ci_amount == 1) {
    .data %>%  
      unite(ci, m_low1, m_upp1, sep = "–", remove = TRUE) 
  }
  # added specifically for PA Inter-quartile range (P25-P75)
  else if (.ci_amount == "q") {
    .data %>%
      unite(q, q_q25, q_q75, sep = "–", remove = TRUE) 
  }
}

################################################################################

# Function to create hux tables
# Arguments description:
# .col_names - value for the column names list

apply_hux <- function(.data, .col_names) {
  .data %>%
    # convert to a hux table using the hux function
    hux() %>%
    # apply a hux theme
    theme_compact() %>% 
    # add column names
    set_contents(1, everywhere, {{ .col_names }}) %>%
    # borders around table
    set_outer_borders() %>%
    # border for header row
    set_top_border(row = 2, everywhere) %>%
    # border for total row
    set_top_border(final(1), everywhere) %>%
    # make the table headings bold (IF NEEDED)
    style_headers(bold = FALSE) %>%
    #OR use: set_bold(row = 1, col = everywhere) %>% 
    # make total row bold
    set_bold(final(1), everywhere) %>%
    # other small tweaks
    set_position("left") %>%
    set_font_size(value = 8) %>%
    set_width(1.2) %>%
    set_align(value = "center") %>%
    set_valign(value = "middle") %>% 
    set_number_format(row=-1, col=-c(1,2), value=1)
}

################################################################################

# Function to prep clean-looking main tables (men, women, both sexes)
# Arguments description:
# .s_amount - amount of sexes in the table (1 = only "Men" or "Women, or 
# "Both sexes", 3 = all three sexes included)
# .s_val - value of the sex (specified for .s_amount = 1)
# .title - title of the table
# .colspan_val - value for the column span (i.e., overall number of columns in the table)

final_m_w_b_tbl <- function(.data, .s_amount, .s_val, .title, .colspan_val) {
  # weighted scripts
  if(.s_amount == 1) {
    .data %>% 
      insert_row({{ .s_val }}, fill = "", colspan = .colspan_val) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      set_font_size(value = 8)
  } 
  else if (.s_amount == 3) {
    .data %>% 
      insert_row(c("Age Group (years)","Men","","","Women","","","Both sexes","","")) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      merge_cells(row = 2, col = c(8:10)) %>% merge_cells(row = 2, col = c(5:7)) %>% 
      merge_cells(row = 2, col = c(2:4)) %>% merge_cells(row = c(2:3), col = 1) %>% 
      set_right_border(row = 2, col = 1, value = 0) %>% 
      set_font_size(value = 8)
  }
  # unweighted scripts
  else if (.s_amount == "unwt_3") {
    .data %>% 
      insert_row(c("Age Group (years)","Men","","Women","","Both sexes","")) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      merge_cells(row = 2, col = c(6:7)) %>% merge_cells(row = 2, col = c(4:5)) %>% 
      merge_cells(row = 2, col = c(2:3)) %>% merge_cells(row = c(2:3), col = 1) %>% 
      set_right_border(row = 2, col = 1, value = 0) %>% 
      set_font_size(value = 8)
  }
}

################################################################################

# Function to prep clean-looking urban/rural tables
# Arguments description:
# .s_amount - amount of sexes in the table (1 = only "Men" or "Women, or 
# "Both sexes", 3 = all three sexes included)
# .s_val - value of the sex (specified for .s_amount = 1)
# .title - title of the table
# .colspan_val - value for the column span (i.e., overall number of columns in the table)

final_u_r_tbl <- function(.data, .s_amount, .s_val, .title, .colspan_val) {
  # weighted scripts
  if(.s_amount == 1) {
    .data %>% 
      # remove second header after joining urban & rural tables
      slice(-5) %>%
      # add urban and rural rows
      insert_row("Urban", after = 1, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 2, everywhere, value = 0) %>% 
      insert_row("Rural", after = 5, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 6, everywhere, value = 0) %>%
      # inserting urban and rural rows removes the right border, so set it up back again
      set_right_border(row = c(2, 6), col = 1) %>% 
      # bold urban and rural rows
      set_bold(row = c(2, 6), everywhere) %>%
      # align left urban and rural
      set_align(row = 2, value = "left") %>%
      set_align(row = 6, value = "left") %>% 
      # move urban/rural headers slightly from the left border
      set_left_padding(row = 2, col = 1, value = 8) %>% 
      set_left_padding(row = 6, col = 1, value = 8) %>% 
      # add title, select sex, prep output
      insert_row({{ .s_val }}, fill = "", colspan = .colspan_val) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      set_font_size(value = 8)
  } 
  else if (.s_amount == 3) {
    .data %>% 
      # remove second header after joining urban & rural tables
      slice(-5) %>%
      # add urban and rural rows
      insert_row("Urban", after = 1, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 2, everywhere, value = 0) %>%
      insert_row("Rural", after = 5, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 6, everywhere, value = 0) %>%
      # inserting urban and rural rows removes the right border, so set it up back again
      set_right_border(row = c(2, 6), col = 1) %>%
      # bold urban and rural rows
      set_bold(row = c(2, 6), everywhere) %>%
      # align left urban and rural
      set_align(row = 2, value = "left") %>%
      set_align(row = 6, value = "left") %>% 
      # move urban/rural headers slightly from the left border
      set_left_padding(row = 2, col = 1, value = 8) %>% 
      set_left_padding(row = 6, col = 1, value = 8) %>% 
      # add title, select sex, prep output
      insert_row(c("Age Group (years)","Men","","","Women","","","Both sexes","","")) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      merge_cells(row = 2, col = c(8:10)) %>% merge_cells(row = 2, col = c(5:7)) %>% 
      merge_cells(row = 2, col = c(2:4)) %>% merge_cells(row = c(2:3), col = 1) %>% 
      set_right_border(row = 2, col = 1, value = 0) %>%
      set_font_size(value = 8)
  }
  # unweighted scripts
  else if (.s_amount == "unwt_3") {
    .data %>% 
      # remove second header after joining urban & rural tables
      slice(-5) %>%
      # add urban and rural rows
      insert_row("Urban", after = 1, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 2, everywhere, value = 0) %>%
      insert_row("Rural", after = 5, fill = "", colspan = .colspan_val) %>%
      set_bottom_border(row = 6, everywhere, value = 0) %>%
      # inserting urban and rural rows removes the right border, so set it up back again
      set_right_border(row = c(2, 6), col = 1) %>%
      # bold urban and rural rows
      set_bold(row = c(2, 6), everywhere) %>%
      # align left urban and rural
      set_align(row = 2, value = "left") %>%
      set_align(row = 6, value = "left") %>% 
      # move urban/rural headers slightly from the left border
      set_left_padding(row = 2, col = 1, value = 8) %>% 
      set_left_padding(row = 6, col = 1, value = 8) %>% 
      # add title, select sex, prep output
      insert_row(c("Age Group (years)","Men","","Women","","Both sexes","")) %>% 
      insert_row({{ .title }}, fill = "", colspan = .colspan_val) %>% 
      set_bold(row = c(1, 2), everywhere) %>%
      set_align(row = c(1, 2), value = "center") %>% 
      set_all_borders(row = c(1, 2), everywhere) %>% 
      merge_cells(row = 2, col = c(6:7)) %>% merge_cells(row = 2, col = c(4:5)) %>% 
      merge_cells(row = 2, col = c(2:3)) %>% merge_cells(row = c(2:3), col = 1) %>% 
      set_right_border(row = 2, col = 1, value = 0) %>% 
      set_font_size(value = 8)
  }
}

################################################################################

# Function for joining individual hux tables into one (men, women, both sexes)
# Arguments description:
# .ci_amount_val - value for the amount of CIs in the table
# .col_names_val - value for the column names list

join_m_w_b <- function(.data, .ci_amount_val, .col_names_val) {
  if(.ci_amount_val == 0) {
    # for unweighted scripts (without CI)
    # men, women, both sexes
    m_w_b <- c("m","w","b")
    tbl_m_w_b <- list(.data$m, .data$w, .data$b) %>% 
      map(~ apply_hux(., .col_names = {{ .col_names_val }})) %>% 
      set_names(m_w_b)
  } else {
    # for weighted scripts (with CI)
    # men, women, both sexes
    m_w_b <- c("m","w","b")
    tbl_m_w_b <- list(.data$m, .data$w, .data$b) %>% 
      map(~ unite_ci(., .ci_amount = {{ .ci_amount_val }}) %>% 
            apply_hux(.col_names = {{ .col_names_val }})) %>% 
      set_names(m_w_b)
  }
  return(tbl_m_w_b)
}

################################################################################

# Function for joining individual hux tables into one (urban, rural disaggregation)
# Arguments description:
# .ci_amount_val - value for the amount of CIs in the table
# .col_names_val - value for the column names list

join_u_r <- function(.data, .ci_amount_val, .col_names_val) {
  if(.ci_amount_val == 0) {
    # for unweighted scripts (without CI)
    # urban, rural
    u_r <- c("m_u","w_u","b_u","m_r","w_r","b_r")
    tbl_u_r <- list(
      # urban
      .data$m_u, .data$w_u, .data$b_u,
      # rural
      .data$m_r, .data$w_r, .data$b_r) %>%
      map(~ apply_hux(., .col_names = {{ .col_names_val }})) %>%
      set_names(u_r)
  } else {
    # for weighted scripts (with CI)
    # urban, rural
    u_r <- c("m_u","w_u","b_u","m_r","w_r","b_r")
    tbl_u_r <- list(
      # urban
      .data$m_u, .data$w_u, .data$b_u,
      # rural
      .data$m_r, .data$w_r, .data$b_r) %>%
      map(~ unite_ci(., .ci_amount = {{ .ci_amount_val }}) %>% 
            apply_hux(.col_names = {{ .col_names_val }})) %>%
      set_names(u_r)
  }
  return(tbl_u_r)
}

################################################################################

