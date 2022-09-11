# WHO STEPS MDA 2021

## Overview

R Project for analysis of the STEPS survey data conducted in the Republic of Moldova, in 2021. The project allows to create a data book, fact sheet in Word and generate forest plots in vector formats.

-   Scripts were written on the basis of Epi Info programmes/scripts and named respectively

-   `STEPS.mdb` data file for MDA is needed for the project and should be placed in the root folder

-   `LoadData.R` script (located in the root folder) is used for manually adjusting the inclusion of disaggregations (by urban/rural and region), project title and authors

-   Code in the project contains comments and functions have descriptions of arguments

-   Some scripts in modules are tailored to the country-specific settings

-   It is suggested to use RStudio software and `stepsMDA2021.Rproj` file to load the project

-   Project has been tested on MacOS 12.3 and Windows 10/11, using R version 4.1.3

-   MS Office with Access is required to be installed for loading the MDB file

    -   Use the 32-bit version of R when MS Office is also 32-bit

## Contents

The WHO STEPS in MDA includes the following modules:

1.  Demographic Information

2.  Tobacco Use

3.  Tobacco Policy

4.  Alcohol Consumption

5.  Diet (abridged version)

6.  Physical Activity

7.  History of Raised Blood Pressure

8.  History of Diabetes

9.  History of Raised Total Cholesterol

10. History of Cardiovascular Diseases

11. Lifestyle Advice

12. Cervical Cancer Screening

13. Physical Measurements

14. Biochemical Measurements

15. Cardiovascular disease risk

16. Summary of Combined Risk Factors

17. Oral Health

18. Depression

## Usage

### Data book

Individual Rmd files are used to create Word documents for each module (located in `DataBook/` folder).

### Fact sheet

The `factsheet.R` script (located in `FactSheet/` folder) is used for creating a Word document out of CSV files that are written to the folder when Rmd files are run.

### Forest plots

The `make_plots.R` script (located in `Plots/` folder) is used for creating forest plots in PDF, SVG and EMF formats. The script creates an Rds data file with a function, containing all modules, loads the mapping XLSX of included indicators, and generates subsequent plots with a function. The XLSX mapping spreadsheet (located in `Plots/` folder) is used to manually adjust which indicators are included for plotting, using `include_in_analysis` variable (TRUE or FALSE).

## Getting help

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/WorldHealthOrganization/stepsMDA2021/issues).
