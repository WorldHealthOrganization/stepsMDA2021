################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# Set a variable for the dynamic author in RMD files
set_author <- "WHO NCD Office Moscow"
# Set a variable for the dynamic title in RMD files
set_title <- "STEPS MDA Data Book"

################################################################################

# INCLUSION OF URBAN/RURAL AND REGION
# if TRUE - included
# if FALSE - excluded

# Write into the global environment 
.u_r_reg <<- TRUE
# OR
# .GlobalEnv$.u_r_reg <- TRUE

# check the object
.u_r_reg

################################################################################

# R PACKAGES
list_of_packages <- c("here", "tidyverse", "RODBC", "odbc", "Hmisc", "rlang", "srvyr", 
                      "survey", "ggforce", "devEMF", "ggsci", "huxtable", "fs", "flextable", 
                      "rmarkdown", "knitr", "magrittr", "plyr", "readxl", "svglite")

# Checking if already installed packages contain the required packages
install.packages(setdiff(list_of_packages, rownames(installed.packages()))) 

################################################################################

# LOADING AND CLEANING DATA

# Identify project's working directory
library(here)
path <- here()

# Load other main packages
library(tidyverse)

################################################################################

# LOAD DATA 

# Specify the name of STEPS MDB file 
steps_mdb_name <- "STEPS.mdb"

# Load data tables depending on the Operating System
if(.Platform$OS.type == "unix") {
  # MacOS
  library(Hmisc) # needed to load mdb data in MacOS
  data1 <- mdb.get(steps_mdb_name, tables = "data1")
  data2 <- mdb.get(steps_mdb_name, tables = "data2")
} else {
  # Windows
  # check for Microsoft Access Drivers in Windows 
  library(odbc)
  odbc_drivers <- odbcListDrivers() %>% 
    # create mdb_driver variable and assign "mdb" to each to include all possible drivers 
    mutate(mdb_driver = ifelse(name=="Microsoft Access Driver (*.mdb)", "mdb", 
                               ifelse(name=="Microsoft Access Driver (*.mdb, *.accdb)", "mdb", 
                                      ifelse(name=="Driver do Microsoft Access (*.mdb)", "mdb", 
                                             ifelse(name=="Microsoft Access-Treiber (*.mdb)", "mdb", NA))))) %>% 
    filter(!is.na(mdb_driver))
  
  # subset to only unique MDB drivers
  unique_mdb <- unique(odbc_drivers$name)
  
  # open connection based on the available driver and fetch tables
  library(RODBC)
  channel <- odbcDriverConnect(paste0("Driver={", unique_mdb[1], "};DBQ=", path, "/", steps_mdb_name))
  data1 <- sqlFetch(channel, "data1", as.is = TRUE)
  data2 <- sqlFetch(channel, "data2", as.is = TRUE)
  odbcClose(channel)
}

################################################################################

# MERGE DATASETS

intersect(names(data1), names(data2))
# NOTE: MDA used id variable for merging data1 and data2
data <- merge(data1, data2, by="id")

################################################################################

# CLEAN MERGED DATA

# Clean column names to lower case
data <- data %>% rename_all(tolower)

################################################################################

# Core variable names that are used in the analysis
# data_names <- c("sex","valid","wstep1","wstep2","wstep3","psu","stratum",
#                 "agerange","agerange2","ur","region")

# Check which names are not in the data before proceeding
# if(!all(i <- rlang::has_name(data, data_names))) {
#   warning(sprintf(
#     "%s doesn't contain: %s",
#     deparse(substitute(data)),
#     paste(data_names[!i], collapse=", ")))
# }
# NOTE: in MDA data doesn't contain: agerange2, ur (ur is called urbanrural)

################################################################################

# Checking & remove NA values from wstep1 & wstep2
table(is.na(data$wstep1))
table(is.na(data$wstep2))
table(is.na(data$wstep3))
# NOTE: don't remove NAs all at once - numbers for step1-2 would be impacted by 
# potentially removed NAs from wstep3
data <- data[!(is.na(data$wstep1)),]
data <- data[!(is.na(data$wstep2)),]

################################################################################


data <- data %>% 
  # Add agerange2 (18-39, 40-69) for Urban/Rural disaggregation
  mutate(agerange2 = ifelse(age>=18 & age<=39, "18–39",
                            ifelse(age>=40 & age<=69, "40–69", NA))) %>% 
  filter(!is.na(agerange2)) %>% 
  mutate(agerange2 = factor(agerange2)) %>% 
  # Changing the dash to an en dash in age ranges: "-" to "–"
  # This is needed for the final output tables in the data book and fact sheet
  mutate(across("agerange", str_replace, "-", "–")) %>% 
  # NOTE: agerange should be changed to a factor because sometimes age ranges are
  # dropped if there are 0 values in data and .drop = FALSE in group_by
  # doesn't work if it's not a factor.
  # The same applies to variables agerange2, region, sex and ur
  mutate(agerange = factor(agerange)) %>% 
  mutate(region = factor(region)) %>%
  mutate(sex = factor(sex)) %>%
  # Add ur variable as it is called urbanrural in MDA MDB file
  mutate(ur = urbanrural) %>%
  mutate(ur = factor(ur)) %>%
  # Recode values in ur with dplyr
  mutate(ur = recode(ur, R = "Rural", U = "Urban"))

# Check vars
unique(data$agerange)
unique(data$agerange2)
unique(data$sex)
unique(data$ur)


# Including only those respondents that are valid==1
data <- subset(data, valid==1)
dim(data)
table(data$valid)

################################################################################

# Additional recoding of variables for MDA 
# to work with original scripts based on Epi Info programs

data <- data %>%
  rename(
    # electronic cigarettes
    et1=ec1, et2=ec2, et3=ec3, et4=ec4, et4type=ec4type,
    # heated tobacco products were excluded and "other" shifted
    t5g=t5f, t5gw=t5fw) %>%
  mutate(
    # add the var t5f & t5fw for tsmoketype to work properly
    t5f=NA, t5fw=NA)

# MDA fixing MDB dataset (for Windows users)
# NOTE: htp4 variable was not set to numeric by default

# class(data$htp4)
data <- data %>% 
  mutate(htp4 = as.numeric(htp4))

################################################################################



