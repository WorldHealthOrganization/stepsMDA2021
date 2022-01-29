################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# "Age group by gender"

################################################################################

# Load cleaned data

source("LoadData.R", encoding="UTF-8")

################################################################################

library(srvyr)

# Specifying design

STEPSClean <- data %>% as_survey_design(ids=psu, nest=TRUE)

################################################################################

# Load functions

source("functions.R", encoding="UTF-8")

################################################################################

# list function doesn't produce proper numbers for urban/rural
# cagesex_list_long <- tbls_pct_summary(
#   .data = STEPSClean, .variable = NULL, .cln = FALSE, .cln_val = FALSE, .fun = summary_pct_unwt)
# 
# STEPSClean %>% group_by(agerange, sex) %>% summary_pct_unwt()
# 
# cagesex_list_long <- list(
#   STEPSClean %>% group_by(agerange, sex) %>% summary_pct_unwt(),
#   STEPSClean %>% group_by(sex) %>% summary_pct_unwt() %>% 
#     mutate(agerange = factor("18–69"), .before = 1)
# )
  

cagesex_summary <- function() {
  
  if(.u_r_reg == FALSE) {
    # without urban/rural and regional disaggregation
    # MEN & WOMEN
    cagesex_m_w <- STEPSClean %>% group_by(agerange, sex) %>% summary_pct_unwt() %>% ungroup()
    cagesex_m <- filter(cagesex_m_w, sex == "Men") %>% select(1,3,5)
    cagesex_w <- filter(cagesex_m_w, sex == "Women") %>% select(1,3,5) 
    
    # MEN & WOMEN TOTALS
    cagesex_m_w_t <- STEPSClean %>% group_by(sex) %>% summary_pct_unwt() %>% 
      ungroup() %>% mutate(agerange = factor("18–69"), .before = 1)
    cagesex_m_t <- filter(cagesex_m_w_t, sex == "Men") %>% select(1,3,5)
    cagesex_w_t <- filter(cagesex_m_w_t, sex == "Women") %>% select(1,3,5) 
    
    # MEN & WOMEN TBLS JOINED 
    cagesex_m_t_j <- rbind(cagesex_m, cagesex_m_t) 
    cagesex_w_t_j <- rbind(cagesex_w, cagesex_w_t) 
    
    # BOTH SEXES
    cagesex_b <- STEPSClean %>% group_by(agerange) %>% summary_pct_unwt() %>% ungroup() 
    cagesex_b_t <- STEPSClean %>% summary_pct_unwt() %>%  
      ungroup() %>% mutate(agerange = factor("18–69"), .before = 1)
    # BOTH SEXES TBLS JOINED
    cagesex_b_t_j <- rbind(cagesex_b, cagesex_b_t) %>% select(-3)
    
    # joining all above tables into one output
    m_w_b <- c("m","w","b")
    cagesex <- list(
      # men, women, both
      cagesex_m_t_j, cagesex_w_t_j, cagesex_b_t_j) %>% 
      setNames(m_w_b)
    return(cagesex)
  } 
  else {
    # MEN & WOMEN
    cagesex_m_w <- STEPSClean %>% group_by(agerange, sex) %>% summary_pct_unwt() %>% ungroup()
    cagesex_m <- filter(cagesex_m_w, sex == "Men") %>% select(1,3,5)
    cagesex_w <- filter(cagesex_m_w, sex == "Women") %>% select(1,3,5) 
    
    # MEN & WOMEN TOTALS
    cagesex_m_w_t <- STEPSClean %>% group_by(sex) %>% summary_pct_unwt() %>% 
      ungroup() %>% mutate(agerange = factor("18–69"), .before = 1)
    cagesex_m_t <- filter(cagesex_m_w_t, sex == "Men") %>% select(1,3,5)
    cagesex_w_t <- filter(cagesex_m_w_t, sex == "Women") %>% select(1,3,5) 
    
    # MEN & WOMEN TBLS JOINED 
    cagesex_m_t_j <- rbind(cagesex_m, cagesex_m_t) 
    cagesex_w_t_j <- rbind(cagesex_w, cagesex_w_t) 
    
    # BOTH SEXES
    cagesex_b <- STEPSClean %>% group_by(agerange) %>% summary_pct_unwt() %>% ungroup() 
    cagesex_b_t <- STEPSClean %>% summary_pct_unwt() %>%  
      ungroup() %>% mutate(agerange = factor("18–69"), .before = 1)
    # BOTH SEXES TBLS JOINED
    cagesex_b_t_j <- rbind(cagesex_b, cagesex_b_t) %>% select(-3)
    
    ################################################################################
    
    # URBAN - MEN & WOMEN
    cagesex_m_w_u <- STEPSClean %>% filter(ur == "Urban") %>% 
      group_by(agerange2, sex) %>% summary_pct_unwt() %>% ungroup()
    cagesex_m_u <- filter(cagesex_m_w_u, sex == "Men") %>% select(c("agerange2","n","m"))
    cagesex_w_u <- filter(cagesex_m_w_u, sex == "Women") %>% select(c("agerange2","n","m"))
    
    # URBAN TOTAL - MEN & WOMEN
    cagesex_m_w_t_u <- STEPSClean %>% filter(ur == "Urban") %>% group_by(sex) %>% 
      summary_pct_unwt() %>% ungroup() %>% mutate(agerange2 = factor("18–69"), .before = 1)
    cagesex_m_t_u <- filter(cagesex_m_w_t_u, sex == "Men") %>% select(c("agerange2","n","m"))
    cagesex_w_t_u <- filter(cagesex_m_w_t_u, sex == "Women") %>% select(c("agerange2","n","m"))
    
    # URBAN TBLS JOINED
    cagesex_m_t_u_j <- rbind(cagesex_m_u, cagesex_m_t_u) 
    cagesex_w_t_u_j <- rbind(cagesex_w_u, cagesex_w_t_u) 
    
    # RURAL - MEN & WOMEN
    cagesex_m_w_r <- STEPSClean %>% filter(ur == "Rural") %>% 
      group_by(agerange2, sex) %>% summary_pct_unwt() %>% ungroup()
    cagesex_m_r <- filter(cagesex_m_w_r, sex == "Men") %>% select(c("agerange2","n","m"))
    cagesex_w_r <- filter(cagesex_m_w_r, sex == "Women") %>% select(c("agerange2","n","m"))
    
    # RURAL TOTAL - MEN & WOMEN
    cagesex_m_w_t_r <- STEPSClean %>% filter(ur == "Rural") %>% group_by(sex) %>% 
      summary_pct_unwt() %>% ungroup() %>% mutate(agerange2 = factor("18–69"), .before = 1)
    cagesex_m_t_r <- filter(cagesex_m_w_t_r, sex == "Men") %>% select(c("agerange2","n","m"))
    cagesex_w_t_r <- filter(cagesex_m_w_t_r, sex == "Women") %>% select(c("agerange2","n","m"))
    
    # RURAL TBLS JOINED
    cagesex_m_t_r_j <- rbind(cagesex_m_r, cagesex_m_t_r) 
    cagesex_w_t_r_j <- rbind(cagesex_w_r, cagesex_w_t_r) 
    
    # URBAN - BOTH SEXES
    cagesex_b_u <- STEPSClean %>% filter(ur == "Urban") %>% group_by(agerange2) %>% 
      summary_pct_unwt() %>% ungroup() 
    # URBAN TOTAL - BOTH SEXES
    cagesex_b_t_u <- STEPSClean %>% filter(ur == "Urban") %>% 
      summary_pct_unwt() %>%  
      mutate(agerange2 = factor("18–69"), .before = 1)
    cagesex_b_t_u_j <- rbind(cagesex_b_u, cagesex_b_t_u) %>% select(-3)
    
    # RURAL - BOTH SEXES
    cagesex_b_r <- STEPSClean %>% filter(ur == "Rural") %>% group_by(agerange2) %>% 
      summary_pct_unwt() %>% ungroup() 
    # RURAL TOTAL - BOTH SEXES
    cagesex_b_t_r <- STEPSClean %>% filter(ur == "Rural") %>% 
      summary_pct_unwt() %>%  
      mutate(agerange2 = factor("18–69"), .before = 1)
    cagesex_b_t_r_j <- rbind(cagesex_b_r, cagesex_b_t_r) %>% select(-3)
    
    ################################################################################
    
    # REGION - BOTH SEXES
    cagesex_b_reg <- STEPSClean %>% group_by(region) %>% summary_pct_unwt() %>% ungroup()
    cagesex_b_t_reg <- STEPSClean %>% summary_pct_unwt() %>%  
      ungroup() %>% mutate(region = "Total", .before = 1)
    # REGION - BOTH SEXES TBLS JOINED
    cagesex_b_t_reg_j <- rbind(cagesex_b_reg, cagesex_b_t_reg) %>% select(-3)
    
    # joining all above tables into one output
    m_w_b_u_r_reg <- c("m","w","b","m_u","w_u","b_u","m_r","w_r","b_r","b_reg")
    cagesex <- 
      list(
        # men, women, both
        cagesex_m_t_j, cagesex_w_t_j, cagesex_b_t_j,
        # urban (men, women, both)
        cagesex_m_t_u_j, cagesex_w_t_u_j, cagesex_b_t_u_j,
        # rural (men, women, both)
        cagesex_m_t_r_j, cagesex_w_t_r_j, cagesex_b_t_r_j,
        # region (both only)
        cagesex_b_t_reg_j) %>% setNames(m_w_b_u_r_reg)
    
    return(cagesex)
  }
}

cagesex <- cagesex_summary()

################################################################################

