################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# LIST OF FUNCTIONS IN THIS SCRIPT
# 1. summary_mn - core for means (weighted)
# 1.1 summary_mn_unwt - unweighted
# 2. summary_pct - core for percentages (weighted)
# 2.1 summary_pct_unwt - unweighted
# 3. pa_summary_mn - means for Physical Activity module (no rounding of m_upp)

# 4. tbls_mn_summary - calculate means (including both parts: age range rows & total row)
# 5. tbls_pct_summary - calculate percentages (inc. both parts: age range rows & total row)
# 6. fs_summary - create FactSheet summary
# 7. sort_rename_vars - pivot the table (for percentages with more than two answer choices) 
# and rename columns for standardization to produce the output used in DataBook
# 8. tbls_list_split - split the original list (by sex, urban/rural, region) from 
# the summary function (tbls_mn_summary or tbls_pct_summary) into smaller lists 
# for joining later with the join functions in DataBook
# 9. forestplot_steps - create individual forest plots

################################################################################

# Load necessary packages
library(srvyr) # load this package as a general rule if not present in each script
library(tidyr) # for using the unite function (where applicable)
library(purrr) # for using the map function (where applicable)

################################################################################

# FUNCTION FOR SUMMARIZE MEAN IN SRVYR
# Arguments description:
# .data - name of the data frame
# .var - variable name 
summary_mn <- function(.data, .var) {
  srvyr::summarise(
    .data, 
    # Calculating n for individual answer choices
    n = unweighted(n()), 
    # Calculating standard error, confidence intervals and design effect
    m = survey_mean({{ .var }}, vartype = c("ci","se"), deff = TRUE)) %>% 
    # Rounding possible negative numbers to zero
    mutate(m_low = ifelse(m_low <0, 0, m_low)) %>% 
    # Convert NA values to 0 in the output for a clean look
    mutate(m_low = ifelse(is.na(m_low), 0, m_low)) %>% 
    mutate(m_upp = ifelse(is.na(m_upp), 0, m_upp)) %>% 
    mutate(m_deff = ifelse(is.na(m_deff), 0, m_deff))
}

################################################################################

# UNWEIGHTED MEAN
# Arguments description:
# .var - variable name 
summary_mn_unwt <- function(.data, .var) {
  dplyr::summarise(
    .data, 
    n = n(),
    m = mean({{ .var }})) 
}

################################################################################

# FUNCTION FOR SUMMARIZE PCT IN SRVYR
summary_pct <- function(.data) {
  srvyr::summarise(
    .data, 
    # Calculating n for individual answer choices
    n = unweighted(n()), 
    # Calculating standard error, confidence intervals and design effect
    m = survey_mean(vartype = c("ci","se"), deff = TRUE)) %>% 
    # Rounding possible negative numbers to zero
    mutate(m_low = ifelse(m_low<0, 0, m_low)) %>% 
    # Calculating a sum of all n answer choices
    mutate(n_sum = sum(n), .after = n) %>% 
    # Convert specific columns to % (except m_deff)
    mutate(across(c(m,m_low,m_upp,m_se), ~100*.x)) %>% 
    # Convert NA values to 0 in the output for a clean look
    mutate(m_low = ifelse(is.na(m_low), 0, m_low)) %>% 
    mutate(m_upp = ifelse(is.na(m_upp), 0, m_upp)) %>% 
    mutate(m_deff = ifelse(is.na(m_deff), 0, m_deff))
}

################################################################################

# UNWEIGHTED PCT
summary_pct_unwt <- function(.data) {
  dplyr::summarise(
    .data, 
    n = n()) %>%
    mutate(m = (n / sum(n))*100) %>% 
    # Calculating a sum of all n answer choices
    mutate(n_sum = sum(n), .after = n)
}

################################################################################

# FUNCTION FOR SUMMARIZE *MEDIAN* IN SRVYR SPECIFICALLY FOR PHYSICAL ACTIVITY AND OTHER SUCH CASES
# Arguments description:
# .var - variable name 
pa_summary_md <- function(.data, .var) {
  srvyr::summarise(
    .data, 
    # Calculating n for individual answer choices
    n = unweighted(n()), 
    md = survey_median({{ .var }}, vartype = c("ci","se")),
    q = survey_quantile({{ .var }}, quantiles = c(0.25, 0.75))) 
}

################################################################################

# MEANS

# A FUNCTION FOR COVERING BOTH PARTS (AGERANGE ROWS + TOTAL ROW)
# MEN, WOMEN & BOTH SEXES
# can be used with summary_mn or pa_summary_md functions as the .fun argument 
# Arguments description:
# .variable - variable name
# .cln - clean (CLN) variable as used in EpiInfo programme scripts (up to three cln variables)
# .cln_val - value for each clean variable
# .agerange_var - name of the age range variable (set to agerange by default)
# .agerange_u_r_var - for urban, rural disaggregation (set to agerange2 by default)
# .fun - summary function either for mean or median (set to summary_mn by default)

tbls_mn_summary <- function(.data = STEPSClean, .variable, .cln = cln, 
                            .cln_val = 1, .cln2 = FALSE, .cln2_val = FALSE, 
                            .cln3 = FALSE, .cln3_val = FALSE, 
                            .agerange_var = agerange, .agerange_u_r_var = agerange2, 
                            .fun = summary_mn) {
  
  if(.u_r_reg == FALSE) {
    # without urban/rural and regional disaggregation
    ############################################################################
    # MEN, WOMEN
    tbl_m_w <- .data %>% 
      # Filter potential NAs in .variable (not inside summarize, as it impacts n-variable in the output)
      # (Copies the same logic as in EpiInfo)
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, sex, .drop = FALSE) %>% {{ .fun }}({{ .variable }})
    # MEN, WOMEN - TOTALS
    tbl_m_w_t <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1)
    # MEN, WOMEN - JOINED
    tbl_m_w_t_j <- rbind(tbl_m_w, tbl_m_w_t)
    
    # BOTH SEXES
    tbl_b <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - TOTAL
    tbl_b_t <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      # probably group_by is unnecessary
      # group_by(.drop = FALSE) %>% 
      {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - JOINED
    tbl_b_t_j <- rbind(tbl_b, tbl_b_t)
    
    # MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b <- rbind(tbl_m_w_t_j, tbl_b_t_j) %>% ungroup()
    
    return(tbls_m_w_b)
  } 
  else {
    ############################################################################
    # 1
    # MEN, WOMEN
    tbl_m_w <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, sex, .drop = FALSE) %>% {{ .fun }}({{ .variable }})
    # MEN, WOMEN - TOTALS
    tbl_m_w_t <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1)
    # MEN, WOMEN - JOINED
    tbl_m_w_t_j <- rbind(tbl_m_w, tbl_m_w_t)
    
    # BOTH SEXES
    tbl_b <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - TOTAL
    tbl_b_t <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      # probably group_by is unnecessary
      # group_by(.drop = FALSE) %>% 
      {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - JOINED
    tbl_b_t_j <- rbind(tbl_b, tbl_b_t)
    
    # MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b <- rbind(tbl_m_w_t_j, tbl_b_t_j) %>% ungroup()
    
    ############################################################################
    # 2
    # URBAN, RURAL - MEN, WOMEN
    tbl_m_w_u_r <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_u_r_var }}, sex, ur, .drop = FALSE) %>% {{ .fun }}({{ .variable }})
    # TOTALS
    tbl_m_w_t_u_r <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, ur, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_u_r_var }}" := factor("18–69"), .before = 1)
    # JOINED
    tbls_m_w_u_r_j <- rbind(tbl_m_w_u_r, tbl_m_w_t_u_r)
    
    # URBAN, RURAL - BOTH SEXES
    tbl_b_u_r <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_u_r_var }}, ur, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # TOTALS
    tbl_b_t_u_r <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(ur, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate("{{ .agerange_u_r_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # JOINED
    tbls_b_u_r_j <- rbind(tbl_b_u_r, tbl_b_t_u_r)
    
    # URBAN, RURAL - MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b_u_r_j <- rbind(tbls_m_w_u_r_j, tbls_b_u_r_j) %>% ungroup()
    
    ############################################################################
    # 3
    # REGION (BOTH SEXES ONLY)
    tbl_b_reg <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(region, .drop = FALSE) %>% {{ .fun }}({{ .variable }}) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # TOTAL
    tbl_b_t_reg <- .data %>% 
      filter(!is.na({{ .variable }}), {{ .cln }} == {{ .cln_val }}, 
             {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      # probably group_by is unnecessary 
      # group_by(.drop = FALSE) %>% 
      {{ .fun }}({{ .variable }}) %>% 
      mutate(region = factor("Total"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # JOINED
    tbl_b_t_reg_j <- rbind(tbl_b_reg, tbl_b_t_reg) %>% ungroup()
    
    ############################################################################
    
    # joining all main tables into one list
    m_w_b_u_r_reg <- c("m_w_b","m_w_b_u_r","b_reg")
    tbls <- 
      list(
        # men, women, both
        tbls_m_w_b,
        # urban, rural (men, women, both)
        tbls_m_w_b_u_r_j,
        # region (both only)
        tbl_b_t_reg_j) %>% setNames(m_w_b_u_r_reg)
    
    return(tbls)
  }
}

################################################################################

# PERCENTAGES

# A FUNCTION FOR COVERING BOTH PARTS (AGERANGE ROWS + TOTAL ROW)
# MEN, WOMEN & BOTH SEXES
# Track changes/updates:
# - covers CVDrisk.R with only two age ranges (hence adjustable .agerange_var argument in the function)
# - includes cln2 & cln3 (variable and value) and set to FALSE (skipped) by default
# - function is set to pct by default
# - updated to {{ .fun }} for summary_pct() (set by default)

# Arguments description:
# .variable - variable name
# .cln - clean (CLN) variable as used in EpiInfo programme scripts (up to three cln variables)
# .cln_val - value for each clean variable
# .agerange_var - name of the age range variable (set to agerange by default)
# .agerange_u_r_var - for urban, rural disaggregation (set to agerange2 by default)
# .fun - summary function for percentages (set to summary_pct by default)

tbls_pct_summary <- function(.data = STEPSClean, .variable, .cln = cln, 
                             .cln_val = 1, .cln2 = FALSE, .cln2_val = FALSE, 
                             .cln3 = FALSE, .cln3_val = FALSE,
                             .agerange_var = agerange, .agerange_u_r_var = agerange2, 
                             .fun = summary_pct) {
  
  if(.u_r_reg == FALSE) {
    # without urban/rural and regional disaggregation
    ############################################################################
    # MEN, WOMEN
    tbl_m_w <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, sex, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}()
    # MEN, WOMEN - TOTALS
    tbl_m_w_t <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1)
    # MEN, WOMEN - JOINED
    tbl_m_w_t_j <- rbind(tbl_m_w, tbl_m_w_t)
    
    # BOTH SEXES
    tbl_b <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - TOTAL
    tbl_b_t <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - JOINED
    tbl_b_t_j <- rbind(tbl_b, tbl_b_t)
    
    # MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b <- rbind(tbl_m_w_t_j, tbl_b_t_j) %>% ungroup()
    
    return(tbls_m_w_b)
  } 
  else {
    ############################################################################
    # 1
    # MEN, WOMEN
    tbl_m_w <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, sex, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}()
    # MEN, WOMEN - TOTALS
    tbl_m_w_t <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1)
    # MEN, WOMEN - JOINED
    tbl_m_w_t_j <- rbind(tbl_m_w, tbl_m_w_t)
    
    # BOTH SEXES
    tbl_b <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_var }}, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - TOTAL
    tbl_b_t <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # BOTH SEXES - JOINED
    tbl_b_t_j <- rbind(tbl_b, tbl_b_t)
    
    # MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b <- rbind(tbl_m_w_t_j, tbl_b_t_j) %>% ungroup()
    
    ############################################################################
    # 2
    # URBAN, RURAL - MEN, WOMEN
    tbl_m_w_u_r <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_u_r_var }}, sex, ur, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}()
    # TOTALS
    tbl_m_w_t_u_r <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(sex, ur, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_u_r_var }}" := factor("18–69"), .before = 1)
    # JOINED
    tbls_m_w_u_r_j <- rbind(tbl_m_w_u_r, tbl_m_w_t_u_r)
    
    # URBAN, RURAL - BOTH SEXES
    tbl_b_u_r <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .agerange_u_r_var }}, ur, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # TOTALS
    tbl_b_t_u_r <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(ur, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate("{{ .agerange_u_r_var }}" := factor("18–69"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # JOINED
    tbls_b_u_r_j <- rbind(tbl_b_u_r, tbl_b_t_u_r)
    
    # URBAN, RURAL - MEN, WOMEN, BOTH SEXES - JOINED
    tbls_m_w_b_u_r_j <- rbind(tbls_m_w_u_r_j, tbls_b_u_r_j) %>% ungroup()
    
    ############################################################################
    # 3
    # REGION (BOTH SEXES ONLY)
    tbl_b_reg <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by(region, {{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # TOTAL
    tbl_b_t_reg <- .data %>% 
      filter({{ .cln }} == {{ .cln_val }}, {{ .cln2 }} == {{ .cln2_val }}, {{ .cln3 }} == {{ .cln3_val }}) %>% 
      group_by({{ .variable }}, .drop = FALSE) %>% {{ .fun }}() %>% 
      mutate(region = factor("Total"), .before = 1) %>% 
      mutate(sex = factor("Both sexes"), .before = 2)
    # JOINED
    tbl_b_t_reg_j <- rbind(tbl_b_reg, tbl_b_t_reg) %>% 
      ungroup() # ungroup for dropping columns when/if needed
    
    ############################################################################
    
    # joining all main tables into one list
    m_w_b_u_r_reg <- c("m_w_b","m_w_b_u_r","b_reg")
    tbls <- 
      list(
        # men, women, both
        tbls_m_w_b,
        # urban, rural (men, women, both)
        tbls_m_w_b_u_r_j,
        # region (both only)
        tbl_b_t_reg_j) %>% setNames(m_w_b_u_r_reg)
    
    return(tbls)
  }
}

################################################################################

# FACTSHEET FUNCTION
# Arguments description:
# .data - specify the data frame
# .sel_var - selection of variables for the Fact Sheet
# .name_val - enter column names for either men, women or both sexes
# .pct - toggle option for adding of "%" sign to values

fs_summary <- function(.data, .sel_var, .name_val, .pct) {
  
  if (missing(.pct)) {
    .data %>% 
      # select note: use column numbers instead of names when they are too long
      select({{ .sel_var }}) %>%
      # renaming to bring every column to a standard naming
      # some df have longer names and some df don't, hence the following renaming
      rename(m=1, m_low=2, m_upp=3) %>% 
      ##########################################################################
      # UPDATE
      # Keeping zeros after decimal point 
      mutate(m = sprintf("%.1f", m)) %>% 
      mutate(m_low = sprintf("%.1f", m_low)) %>% 
      mutate(m_upp = sprintf("%.1f", m_upp)) %>% 
      ##########################################################################
      # joining upper & lower CI into one column
      unite(ci, m_low, m_upp, sep = "–", remove = TRUE) %>% 
      # adding parenthesis to CI
      mutate(ci = replace(ci, ci==ci, paste0("(", ci, ")"))) %>% 
      # joining the above together
      unite({{ .name_val }}, m, ci, sep = " ", remove = TRUE)
  } 
  else {
    .data %>% 
      # select note: use column numbers instead of names when they are too long
      select({{ .sel_var }}) %>%
      # renaming to bring every column to a standard naming
      # some df have longer names and some df don't, hence the following renaming
      rename(m=1, m_low=2, m_upp=3) %>% 
      ##########################################################################
      # UPDATE
      # Keeping zeros after decimal point 
      mutate(m = sprintf("%.1f", m)) %>% 
      mutate(m_low = sprintf("%.1f", m_low)) %>% 
      mutate(m_upp = sprintf("%.1f", m_upp)) %>% 
      ##########################################################################
      # joining upper & lower CI into one column
      unite(ci, m_low, m_upp, sep = "–", remove = TRUE) %>% 
      # adding % sign to pct column
      mutate(m = replace(m, m==m, paste0(m, "%"))) %>% 
      # adding parenthesis to CI
      mutate(ci = replace(ci, ci==ci, paste0("(", ci, ")"))) %>% 
      # joining the above together
      unite({{ .name_val }}, m, ci, sep = " ", remove = TRUE)
  }
  
}

################################################################################

# PCT FOR MANY VARIABLES (when you have to pivot the table to produce the output)
# This function is used inside the following function: tbls_list_split
# Arguments description:
# .var - variable name
# .vars_amount - amount of variables (i.e., number of answer choices in the question) in the table/data frame
# .var_val - value (i.e., answer choice) of the variable. When there are two answer choices, then
# typically the final output (in DataBook) will have only one of those two answers. In this case, select 
# that required answer using the .var_val argument.

sort_rename_vars <- function(.data, .var, .vars_amount, .var_val) {

  # WEIGHTED DATA
  ##############################################################################
  # FOR PERCENTAGES
  if((.vars_amount == 7) & ("m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = c(m, m_low, m_upp, m_se, m_deff)) %>%
      select(c(1,2,3,10,17,4,11,18,5,12,19,6,13,20,7,14,21,8,15,22,9,16,23)) %>%
      rename(m1=3, m_low1=4, m_upp1=5,
             m2=6, m_low2=7, m_upp2=8,
             m3=9, m_low3=10, m_upp3=11,
             m4=12, m_low4=13, m_upp4=14,
             m5=15, m_low5=16, m_upp5=17,
             m6=18, m_low6=19, m_upp6=20,
             m7=21, m_low7=22, m_upp7=23)
  }
  else if ((.vars_amount == 6) & ("m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = c(m, m_low, m_upp, m_se, m_deff)) %>%
      select(c(1,2,3,9,15,4,10,16,5,11,17,6,12,18,7,13,19,8,14,20)) %>%
      rename(m1=3, m_low1=4, m_upp1=5,
             m2=6, m_low2=7, m_upp2=8,
             m3=9, m_low3=10, m_upp3=11,
             m4=12, m_low4=13, m_upp4=14,
             m5=15, m_low5=16, m_upp5=17,
             m6=18, m_low6=19, m_upp6=20)
  }
  else if ((.vars_amount == 5) & ("m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = c(m, m_low, m_upp, m_se, m_deff)) %>%
      select(c(1,2,3,8,13,4,9,14,5,10,15,6,11,16,7,12,17)) %>%
      rename(m1=3, m_low1=4, m_upp1=5,
             m2=6, m_low2=7, m_upp2=8,
             m3=9, m_low3=10, m_upp3=11,
             m4=12, m_low4=13, m_upp4=14,
             m5=15, m_low5=16, m_upp5=17)
  }
  else if ((.vars_amount == 4) & ("m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = c(m, m_low, m_upp, m_se, m_deff)) %>%
      select(c(1,2,3,7,11,4,8,12,5,9,13,6,10,14)) %>%
      rename(m1=3, m_low1=4, m_upp1=5,
             m2=6, m_low2=7, m_upp2=8,
             m3=9, m_low3=10, m_upp3=11,
             m4=12, m_low4=13, m_upp4=14)
  }
  else if (.vars_amount == 3) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = c(m, m_low, m_upp, m_se, m_deff)) %>%
      select(c(1,2,3,6,9,4,7,10,5,8,11)) %>%
      rename(m1=3, m_low1=4, m_upp1=5,
             m2=6, m_low2=7, m_upp2=8,
             m3=9, m_low3=10, m_upp3=11)
  }
  # for questions with two answer choices (one in the output)
  else if (.vars_amount == 2) {
    .data %>% 
      # filter only one answer choice 
      filter({{ .var }} == {{ .var_val }}) %>% 
      # drop columns irrelevant for data book 
      select(-c(n, {{ .var }}, m_se, m_deff)) %>% 
      # prep specific columns for huxtable functions later
      rename(m1=3, m_low1=4, m_upp1=5)
  }
  # FOR MEANS
  else if ((.vars_amount == 0) & (!"md" %in% names(.data)) & ("m_low" %in% names(.data))) {
    .data %>% 
      select(c(1,2,3,4,5)) %>% 
      rename(m1=3, m_low1=4, m_upp1=5)
  }
  # FOR MEDIANS
  else if ((.vars_amount == 0) & ("md" %in% names(.data)) & (!"m_low" %in% names(.data))) {
    .data %>% 
      select(c(1,2,3,7,8))
  }
  # UNWEIGHTED DATA
  ##############################################################################
  else if ((.vars_amount == 7) & (!"m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = m) %>% 
      rename(m1=3, m2=4, m3=5, m4=6, m5=7, m6=8, m7=9)
  }
  else if ((.vars_amount == 6) & (!"m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = m) %>% 
      rename(m1=3, m2=4, m3=5, m4=6, m5=7, m6=8)
  }
  else if ((.vars_amount == 5) & (!"m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = m) %>% 
      rename(m1=3, m2=4, m3=5, m4=6, m5=7)
  }
  else if ((.vars_amount == 4) & (!"m_low" %in% names(.data))) {
    .data %>% 
      # drop "n" column for pct cases to keep only the overall "n_sum" column
      select(-n) %>% 
      # pivot wider from a long format 
      pivot_wider(names_from = {{ .var }}, values_from = m) %>% 
      rename(m1=3, m2=4, m3=5, m4=6)
  }
  # FOR MEANS (UNWT)
  else if ((.vars_amount == 0) & (!"md" %in% names(.data)) & (!"m_low" %in% names(.data))) {
    .data %>% 
      select(c(1,2,3))
  }
}


################################################################################

# Function to prep tables with one variable output and split the original list into smaller lists 
# for joining with join functions for men, women, both sexes and urban/rural, region
# Track changes/updates:
# - includes two user-cases when you need to produce a list of pct tables with 
# one variable/column (out of initial two) and when you need tables with 
# many variables/columns (without dropping columns)
# - includes means as well as percentages
# - includes a check on "m_w_b_u_r" presence in the list of tables (when .u_r_reg <<- TRUE)
# 
# Arguments description:
# .select_var - indicate the needed variable
# .vars_amount_number - amount of variables (i.e., number of answer choices in the question)
# .select_var_val - indicate the value (i.e., answer choice) of the variable (for cases 
# when there are two answers and only one is needed in the output). It is skipped by default (set to FALSE).

tbls_list_split <- function(.data, .select_var, .vars_amount_number, .select_var_val = FALSE) {
  
  # MEN, WOMEN, BOTH SEXES WITH ADDITONAL DISAGGREGATIONS (URBAN, RURAL, REGION)
  # FOR PERCENTAGES
  # ONLY ONE ANSWER CHOICE OUTPUT (2 vars initially)
  if(.vars_amount_number == 2 & (.GlobalEnv$.u_r_reg == TRUE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data$m_w_b, .data$m_w_b$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}, 
                             .var_val = {{ .select_var_val }}))
    # men, women, both sexes - urban, rural - split by sex and ur into a list
    tbls_m_w_b_u_r_bysex <- 
      split(.data$m_w_b_u_r, paste(.data$m_w_b_u_r$sex, .data$m_w_b_u_r$ur)) %>% 
      setNames(c("b_r","b_u","m_r","m_u","w_r","w_u")) %>% 
      map(~ select(., -c(sex,ur)) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}, 
                             .var_val = {{ .select_var_val }}))
    # both sexes - region - cleaned for joining
    tbl_b_reg <- list(.data$b_reg) %>% 
      setNames("b_reg") %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}, 
                             .var_val = {{ .select_var_val }}))
    # merge three above lists into one
    tbls <- c(tbls_m_w_b_bysex, tbls_m_w_b_u_r_bysex, tbl_b_reg)
  } 
  # FOR PERCENTAGES
  # SEVERAL ANSWER CHOICES
  else if (.vars_amount_number > 2 & (.GlobalEnv$.u_r_reg == TRUE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data$m_w_b, .data$m_w_b$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, .vars_amount = {{ .vars_amount_number }}))
    # men, women, both sexes - urban, rural - split by sex and ur into a list
    tbls_m_w_b_u_r_bysex <- 
      split(.data$m_w_b_u_r, paste(.data$m_w_b_u_r$sex, .data$m_w_b_u_r$ur)) %>% 
      setNames(c("b_r","b_u","m_r","m_u","w_r","w_u")) %>% 
      map(~ select(., -c(sex,ur)) %>% 
            sort_rename_vars(., .var = {{ .select_var }}, .vars_amount = {{ .vars_amount_number }}))
    # both sexes - region - cleaned for joining
    tbl_b_reg <- list(.data$b_reg) %>% 
      setNames("b_reg") %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}))
    # merge three above lists into one
    tbls <- c(tbls_m_w_b_bysex, tbls_m_w_b_u_r_bysex, tbl_b_reg)
  }
  # FOR MEANS
  else if (.vars_amount_number == 0 & (.GlobalEnv$.u_r_reg == TRUE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data$m_w_b, .data$m_w_b$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% sort_rename_vars(.vars_amount = {{ .vars_amount_number }})) 
    # men, women, both sexes - urban, rural - split by sex and ur into a list
    tbls_m_w_b_u_r_bysex <- 
      split(.data$m_w_b_u_r, paste(.data$m_w_b_u_r$sex, .data$m_w_b_u_r$ur)) %>% 
      setNames(c("b_r","b_u","m_r","m_u","w_r","w_u")) %>% 
      map(~ select(., -c(sex,ur)) %>% 
            sort_rename_vars(.vars_amount = {{ .vars_amount_number }}))
    # both sexes - region - cleaned for joining
    tbl_b_reg <- list(.data$b_reg) %>% 
      setNames("b_reg") %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.vars_amount = {{ .vars_amount_number }}))
    # merge three above lists into one
    tbls <- c(tbls_m_w_b_bysex, tbls_m_w_b_u_r_bysex, tbl_b_reg)
  }
  ##############################################################################
  # MEN, WOMEN, BOTH SEXES ONLY # without urban/rural and regional disaggregation
  # NOTE: .data that is used below is not a list, as was in above sections
  # FOR PERCENTAGES
  # ONLY ONE ANSWER CHOICE OUTPUT (2 vars initially)
  else if (.vars_amount_number == 2 & (.GlobalEnv$.u_r_reg == FALSE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data, .data$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}, 
                             .var_val = {{ .select_var_val }}))
    # output
    tbls <- c(tbls_m_w_b_bysex)
  }
  # FOR PERCENTAGES
  # SEVERAL ANSWER CHOICES (pivoted wider tables)
  else if (.vars_amount_number > 2 & (.GlobalEnv$.u_r_reg == FALSE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data, .data$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.var = {{ .select_var }}, 
                             .vars_amount = {{ .vars_amount_number }}))
    # output
    tbls <- c(tbls_m_w_b_bysex)
  }
  # FOR MEANS (without a column with answer choices)
  else if (.vars_amount_number == 0 & (.GlobalEnv$.u_r_reg == FALSE)) {
    # men, women, both sexes - split by sex into a list
    tbls_m_w_b_bysex <- 
      split(.data, .data$sex) %>% 
      setNames(c("m","w","b")) %>% 
      map(~ select(., -sex) %>% 
            sort_rename_vars(.vars_amount = {{ .vars_amount_number }}))
    # output
    tbls <- c(tbls_m_w_b_bysex)
  }
  
  return(tbls)
  
}

################################################################################
################################################################################

# Produce forest plots based on ggforestplot package 
# with the additional use of ggforce, devEMF, janitor, and stringr (tidyverse) 
# Arguments description:
# .data - use long-formatted data lists
# .var - use specific variables for "name" argument of ggforestplot
# .var_val - use to specify the value of the selected variable in .var (if needed)
# .title - specify the title of the plot
# .xlab - specify the "xlab" argument of ggforestplot (usually either % or Mean, or Median)
# .many_vals - use when there are many answer choices/values in the selected variable (wide tables in Data Book)
# set to FALSE/skipped by default
# .agerange - use to specify agerange2 or region (if needed), set to "agerange" by default

forestplot_steps <- function(.data, .var, .var_val, .title, .xlab, 
                             .many_vals = FALSE, .agerange = agerange) {
  
  library(ggforestplot)
  library(ggforce)
  library(devEMF)
  
  # for percentages with only one value to present in the output
  # grouped by sex (i.e., name = sex)
  if(!missing(.var) & !missing(.var_val) & .many_vals == FALSE) {
    p <- forestplot(
      df = .data %>% filter({{ .var }} == {{ .var_val }}) %>% 
        mutate("{{ .agerange }}" := fct_reorder({{ .agerange }}, desc({{ .agerange }}))),
      name = sex,
      estimate = m,
      se = m_se,
      colour = {{ .agerange }},
      title = {{ .title }},
      xlab = {{ .xlab }}) + labs(colour = "Age range")
  } 
  # for means with no variable to filter values
  # grouped by sex
  else if(missing(.var) & missing(.var_val) & .many_vals == FALSE) {
    p <- forestplot(
      df = .data %>% mutate("{{ .agerange }}" := fct_reorder({{ .agerange }}, desc({{ .agerange }}))),
      name = sex,
      estimate = m,
      se = m_se,
      colour = {{ .agerange }},
      title = {{ .title }},
      xlab = {{ .xlab }}) + labs(colour = "Age range")
  }
  # for percentages with many values to present in the output
  # can be grouped by various variables
  else if(missing(.var_val) & .many_vals == TRUE) {
    p <- forestplot(
      df = .data %>% mutate("{{ .agerange }}" := fct_reorder({{ .agerange }}, desc({{ .agerange }}))),
      name = {{ .var }},
      estimate = m,
      se = m_se,
      colour = {{ .agerange }},
      title = {{ .title }},
      xlab = {{ .xlab }}) + labs(colour = "Age range") + 
      # group plots by sex
      facet_col(facets = ~sex, scales = "free_y", space = "free")
  }
  
  print(p)
  
  # extract the name of data frame and clean "$" sign from its name
  # and also extra text that's added for urban/rural 
  .df_name <- deparse(substitute(.data)) %>% janitor::make_clean_names() %>% str_replace("_percent.*", "") 
  # or this:
  # %>% str_extract("^.*(?=(_percent_percent))")
  
  # save into three common vectorized formats (for editing plots after saving)
  ggsave(plot = p, filename = paste0(.df_name,".pdf"), device = cairo_pdf, 
         path = here("DataBook","Plots","PDFs"), width = 7, height = 8)
  ggsave(plot = p, filename = paste0(.df_name,".svg"), device = "svg", 
         path = here("DataBook","Plots","SVGs"), width = 7, height = 8)
  # EMF format requires devEMF package
  ggsave(plot = p, filename = paste0(.df_name,".emf"), 
         device = {function(filename, ...) devEMF::emf(file = filename, ...)}, 
         path = here("DataBook","Plots","EMFs"), width = 7, height = 8)
  
}

################################################################################
