# WHO STEPS MDA 2021

## Overview

------------------------------------------------------------------------

R Project for analysis of the STEPS survey data conducted in the Republic of Moldova, in 2021. The project allows to create a data book, fact sheet in Word and generate forest plots in vector formats.

## Contents

------------------------------------------------------------------------

The WHO STEPS in MDA includes the following modules:

1.  Demographic Information

2.  Tobacco Use

3.  Tobacco Policy

4.  Alcohol Consumption

5.  Diet

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

------------------------------------------------------------------------

### Data book

Individual Rmd files are used to create Word documents for each module (located in DataBook folder).

### Fact sheet

The \`factsheet.R\` script is used for creating a Word document out of CSV files that are written to FactSheet folder when Rmd files are run.

### Forest plots

There are two parts for creating plots in PDF, SVG and EMF formats, namely 01_load_save_scripts. R and 02_make_plots.R (located in Plots folder). The first script is used to create an Rds data file, containing all modules, and the second one is used for loading the Rds, adjusting which indicators are inlcuded, and plotting. The XLSX spreadsheet is used for adjusting which indicator is included for plotting (located in Plots folder).

NOTE:

-   Scripts were written on the basis of Epi Info programs/scripts and named respectively

-   MDB data file for MDA is needed for the project, namely STEPS.mdb

-   Some scripts in modules are tailored to the country-specific settings.

## Getting help

------------------------------------------------------------------------

If you encounter a clear bug, please file an issue with a minimal reproducible example on [GitHub](https://github.com/WorldHealthOrganization/stepsMDA2021/issues).
