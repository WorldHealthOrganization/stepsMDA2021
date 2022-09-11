# Instructions

### Initial Setup

Install R (latest version 4.0+) and RStudio Desktop, Open-Source License: <https://cloud.r-project.org/>; <https://www.rstudio.com/products/rstudio/download/>

In Windows, MS Office with Access is required to be installed for loading Microsoft Access database (MDB) files. Make sure to use a 32-bit version of R when MS Office is also 32-bit. In RStudio, follow: Tools ➔ Global Options ➔ General

Make sure that the MDB file is located in the root folder of the project. The MDB must be named "STEPS.mdb", otherwise, the name should be adjusted in `LoadData.R` script. If you use a CSV or XLSX instead of a MDB make sure you adjust `LoadData.R` script accordingly.

Required R packages are installed automatically when `LoadData.R` script is run.

### Starting the Project

Always start the project by loading the Rproj file inside the root folder (`stepsMDA2021.Rproj`).

Start with opening and adjusting `LoadData.R` (located in the root folder). The `LoadData.R` is used for loading and cleaning data before applying functions for creating output tables. The script contains and often needs additional recoding of variables for countries to work with original scripts based on Epi Info programmes.

Decide on whether the inclusion of urban, rural and regional disaggregations is needed or not, by manually changing between `TRUE` (included) or `FALSE` (excluded).

The Project is designed so that the data book is produced first by individual modules, after which, the fact sheet and forest plots can be produced. In `DataBook/` folder, there are individual R Markdown (RMD) files that correspond to sub-folders in `Modules/` folder. Those sub-folders are named the same as RMD files and represent the data book modules. Each module's sub-folder contains R scripts, named according to programme names in Epi Info, as well as `functions/` sub-folder with R functions, written on the basis of Epi Info programmes. The R functions may need to be manually adjusted to country-specific settings, when the STEPS questionnaire has different answer choices.

In order to create an individual Word document for each module, use RMD scripts. The RMD scripts would need to be manually adjusted for country-specific STEPS, when modules are abridged or somehow changed.

### Data Book and Fact Sheet Functions

There are main and supporting functions. The main functions do computations and generate raw table lists, whereas supporting functions create formatted output documents and charts.

To make functions, their arguments and output variables easier to read, their names have been designed to hold a generic meaning and/or what they are used for, such as "m" (men), "w" (women), "b" (both sexes), "u" (urban), "r" (rural), "reg" (region), "ci" (confidence interval), "m_low" (lower bound/limit), "m_upp" (upper bound/limit), "tbl" (table), "mn" (mean), "pct" (percentage), "md" (median), "wt" (weighted), "unwt" (unweighted), "fs" (fact sheet), "vars" (variables), "val" (value), etc.

The `functions.R` script (located in root folder) contains functions used for calculating means, percentages and medians (weighted and unweighted) for each indicator. The functions are used for calculating weighted and unweighted means, percentages and medians for each indicator. Weighted is pre-selected and calculated by default, whereas unweighted is manually indicated as a function argument, which, for example, is used in the Demographic Information module. The main functions do computations and generate raw table lists, whereas supporting functions create formatted output documents and charts. Moreover, the main functions contain core functions and functions built on top of them to filter (e.g., by "clean" (CLN) variables) and group data before calculating various disaggregations, which was implemented via specific arguments. Those functions include age range rows and a total row for the data book tables and return a list of tables, depending on whether urban, rural, and regional disaggregations are included at the beginning of `LoadData.R` script (e.g., "m_w\_b", "m_w\_b_u\_r", "b_reg"). Regional disaggregations are calculated only for both sexes, as in some country-specific settings there are many regions, making additional comparisons between men and women insignificant. The CLN variables are those that represent a specific condition or several conditions as a single variable, on which the data are subsequently filtered to make the analysis more specific or representative.

The main functions include:

1\. `tbls_summary()` - calculates means, percentages, and medians as a list of data frames (including both parts of a data book table: age range rows & total row);

2\. `tbls_list_split()` - splits the original list (by sex, urban, rural, and region) made by the `tbls_summary()` function into smaller lists for joining later with the join function in the data book;

3\. `fs_summary()` - creates a fact sheet summary.

### R Markdown and Forest Plots Functions

The supporting functions are used in RMD scripts for creating Word documents and include:

1\. `unite_ci()` -- unites lower and upper confidence interval values into one CI variable;

2\. `apply_hux()` -- creates hux styled tables (using the huxtable R package);

3\. `join_tbls()` -- creates individual hux tables and joining them into one list (men, women, both sexes, urban, and rural disaggregations);

4\. `final_tbl()` -- applies styling to hux tables generated by `join_tbls()` function (men, women, both sexes, urban, rural, and regional disaggregations).

The `make_plots.R` script (located in `Plots/` folder) is used for creating forest plots for additional analysis of indicators with weighted calculations. The plots are generated by one supporting function:

5\. `forestplot_steps()` -- creates forest plots in vector graphics (PDF, SVG, EMF) and saves them into dedicated folders.
