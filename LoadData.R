################################################################################
# Authors: Sergei Bychkov, Ivo Rakovac
# Copyright: WHO NCD Office
################################################################################

# INCLUSION OF URBAN/RURAL AND REGION
# if TRUE - included
# if FALSE - excluded

.u_r_reg <<- TRUE
#OR
#.GlobalEnv$.u_r_reg <- TRUE
# check
.u_r_reg

################################################################################

# LOADING AND CLEANING DATA

# Identify project's working directory
library(here)

# Load other main packages
library(readr)
library(tidyverse)
# library(dplyr) (part of tidyverse)
# library(stringr) (part of tidyverse)

################################################################################

# LOAD DATA 
# NOTE: SPECIFICALLY FOR MDA
step12 <- read_csv("CSV/mda1_2021_11_26_11_14_35_806467-group-name-removed.csv", na = "n/a") %>%
  # anonymise data
  select(-c(I8, I9)) %>%
  # rename for merging datasets
  rename(QR = QR1)
step3 <- read_csv("CSV/mda3_2021_11_28_06_30_32_334957-group-name-removed.csv", na = "n/a") %>%
  # anonymise data & drop extra matching vars in step3
  select(-c(I8_3, I9_3)) %>%
  # rename for merging datasets
  rename(QR = QR3)

# are there any duplicated QR codes?
any(duplicated(step12$QR))
any(duplicated(step3$QR))

# for how many clusters do we have the data?
length(unique(step12$I2b))

################################################################################

# MERGE DATASETS

# Identical variables
identical_vars <- intersect(names(step12), names(step3))
identical_vars <- identical_vars[!identical_vars %in% "QR"]

# Remove vars from Step3 that are identical to vars in Step1 (before merging)
step3 <- select(step3, -all_of(identical_vars))

# Valid participants from Step3 (signed consent form, I5)
# step3valid <- step3 %>% filter(I5_3==1)

# Merge data from step1 and step3
data <- merge(step12, step3, by="QR", all.x = TRUE)

# Names of the variables in the dataset
# names(data)

# add missing data variables from the country's Excel
# load XLS
library(readxl)
STEPSall_weights_28_11_2021 <- read_excel("From Galina Obreja/STEPSall+weights_28.11.2021.xlsx") %>%
  select(QR, "Urban-Rural", Region, WStep1, WStep2, WStep3) %>%
  rename(ur = "Urban-Rural"
         #,
         #WStep1_Galina = WStep1, WStep2_Galina = WStep2, WStep3_Galina = WStep3
         )

data <- merge(data, STEPSall_weights_28_11_2021, by = "QR")

################################################################################

# CLEAN MERGED DATA

# Calculate age
day <- data$C2a
month <- data$C2b
year <- data$C2c

day[day==77 | day=="n/a"]<-NA
month[month==77 | month=="n/a"]<-NA
year[year==7777 | year=="n/a"]<-NA

dob <- paste0(year,"-",month,"-",day)
dob[is.na(year)]<-NA

data$age <- floor((as.Date(data$I4, format="%Y-%m-%d")-as.Date(dob, format="%Y-%m-%d"))/365.25)
data$age[is.na(data$age)] <- data$C3[is.na(data$age)]
data$age[data$age=="n/a"] <- NA
#table(data$age)

# Add Sex, Stratum, PSU, Valid, wstep1,2,3 (temporary)
data <- data %>%
  mutate(Sex = ifelse(C1==1, "Men",
                      ifelse(C1==2, "Women", NA))) %>%
  mutate(Valid = ifelse(!is.na(age) & age>=18 & age<=69 & !is.na(Sex), 1, 2)) %>%
  # PSU – a variable indicating the sampling units above the household level from the sampling design (cluster)
  mutate(PSU = I2b) %>%
  # Stratum – a variable indicating the level of sampling above PSU or any
  # stratification that was done in the survey design (e.g. regions, urban vs rural).
  # If not needed, simply set Stratum=1 for all records.
  mutate(Stratum = Region) #%>%
  #mutate(wstep1 = 1) %>% mutate(wstep2 = 1) %>% mutate(wstep3 = 1)


# Clean column names to lower case
data <- data %>% rename_all(tolower)

# names(data)
# dim(data)

# Checking & remove NA values from wstep1, etc.
table(is.na(data$wstep1))
table(is.na(data$wstep2))
table(is.na(data$wstep3))
# NOTE: don't remove NAs all at once - numbers for step1-2 would be impacted by 
# potentially removed NAs from step3
# data <- data[!(is.na(data$wstep1)),]
# data <- data[!(is.na(data$wstep2)),]
# data <- data[!(is.na(data$wstep3)),]

################################################################################

# Changing the dash to an en dash in age ranges: "-" to "–"
# This is needed for the final output tables 
unique(data$agerange)
unique(data$agerange2)

data <- data %>% 
  # Add agerange2 (18-39, 40-69) for Urban/Rural disaggregation
  mutate(agerange2 = ifelse(age>=18 & age<=39, "18–39",
                            ifelse(age>=40 & age<=69, "40–69", NA))) %>% 
  filter(!is.na(agerange2)) %>% 
  mutate(agerange2 = factor(agerange2)) %>% 
  # Add agerange with 4 levels
  mutate(agerange = ifelse(age>=18 & age<=29, "18–29",
                           ifelse(age>=30 & age<=44, "30–44",
                                  ifelse(age>=45 & age<=59, "45–59",
                                         ifelse(age>=60 & age<=69, "60–69", NA))))) %>% 
  filter(!is.na(agerange)) %>% 
  mutate(agerange = factor(agerange))

unique(data$agerange)
unique(data$agerange2)
unique(data$sex)

data <- data %>%
  #mutate(across("agerange", str_replace, "-", "–")) %>%
  # NOTE: agerange should be changed to a factor because sometimes age ranges are
  # dropped if there are 0 values in data and .drop = FALSE in group_by
  # doesn't work if it's not a factor.
  # The same applies to variables agerange2, region, sex and ur
  #mutate(agerange = factor(agerange)) %>%
  #mutate(across("agerange2", str_replace, "-", "–")) %>%
  #mutate(agerange2 = factor(agerange2)) %>%
  mutate(region = factor(region)) %>%
  mutate(sex = factor(sex)) %>%
  mutate(ur = factor(ur)) %>%
  # recode values in ur with dplyr
  mutate(ur = recode(ur, R = "Rural", U = "Urban"))

unique(data$ur)

# Including only those respondents that are valid==1
data <- subset(data, valid==1)
dim(data)
table(data$valid)

# NOTE: I5 for step3 has NA values

################################################################################

# MDA recoding of variables

data <- data %>%
  rename(
    # electronic cigarettes
    et1=ec1, et2=ec2, et3=ec3, et4=ec4, et4type=ec4type,
    # heated tobacco products were excluded and "other" shifted
    t5g=t5f, t5gw=t5fw) %>%
  mutate(
    # add the var t5f & t5fw for tsmoketype work properly
    t5f=NA, t5fw=NA
         )


