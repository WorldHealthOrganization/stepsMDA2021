# Instructions

### Initial Setup

Install R (latest version 4.2 or 4.1) and RStudio Desktop (2022.02), Open Source License: <https://cloud.r-project.org/>; <https://www.rstudio.com/products/rstudio/download/>

In Windows, MS Office with Access is required to be installed for loading MDB files. Make sure to use a 32-bit version of R when MS Office is also 32-bit. In RStudio, follow: Tools ➔ Global Options ➔ General

Install packages used in the project by running the following:

`install.packages(c("here", "tidyverse", "RODBC", "odbc", "Hmisc", "rlang", "srvyr", "survey", "ggforce", "devEMF", "huxtable", "fs", "flextable", "rmarkdown"))`

Setup RStudio to evaluate R Markdown (RMD) code chunks from the project. Follow in RStudio: Tools ➔ Global Options ➔ R Markdown ➔ Evaluate chunks in directory ➔ Project

Make sure that the MDB file is located in the root folder of the project. The MDB must be named "STEPS.mdb", otherwise, the name should be adjusted in `LoadData.R` script. If you use a CSV or XLSX instead of a MDB make sure you adjust `LoadData.R` script accordingly.

### Starting the Project

Always start the project by loading the Rproj file inside the root folder (`stepsMDA2021.Rproj`).

Start with opening and adjusting `LoadData.R` (located in the root folder). The `LoadData.R` is used for loading and cleaning data before applying functions for creating output tables. The script contains and often needs additional recoding of variables for countries to work with original scripts based on Epi Info programs.

Decide on whether you need to include urban/rural and regional disaggregations or not, by changing between TRUE (included) or FALSE (excluded).

The Project is designed in such a way that the Data Book is produced first by individual modules, after which, the Fact Sheet and forest plots can be produced. In DataBook folder, there are individual RMD files that correspond to sub-folders in Modules folder. Those sub-folders are named the same as RMD files and represent the Data Book modules. Each module's sub-folder contains R scripts, named according to program names in Epi Info, as well as the functions sub-folder with R functions, written on the basis of Epi Info programs. The R functions may need to be manually adjusted to country-specific settings, when the STEPS questionnaire has different answer choices.

In order to create an individual Word document for each module, you can use RMD scripts. The RMD scripts would need to be manually adjusted for country-specific STEPS, when modules are abridged or somehow changed.

### Data Book and Fact Sheet Functions

There are main and supporting functions. The main functions do calculations and generate raw table lists, whereas supporting functions create formatted output documents and charts.

Function names have been designed to hold a generic meaning of variables and/or what they are used for, such as "m" (male), "f" (female), "b" (both sexes), "u" (urban), "r" (rural), "reg" (region), "ci" (confidence interval), "m_low" (lower bound/limit), "m_upp" (upper bound/limit), "tbl" (table), "mn" (mean), "pct" (percentage), "unwt" (unweighted), "fs" (fact sheet), "pa" (physical activity), "vars" (variables).

The `functions.R` script (located in root folder) contains functions used for calculating means and percentages (weighted and unweighted) for each indicator. Unweighted is indicated by the abbreviation "unwt" at the end of function name. There are core functions and functions built on top of them in order to filter (e.g., by "clean" (CLN) variables) and group data before calculating various disaggregations. Those functions include age range rows and total row for a Data Book and return a list of tables, depending on whether urban/rural and regional disaggregations are included at the beginning of `LoadData.R` script (e.g., "m_w\_b", "m_w\_b_u\_r", "b_reg").

The list of functions:

1\. `summary_mn` - core for means (weighted)

1.1 `summary_mn_unwt` - unweighted

2\. `summary_pct` - core for percentages (weighted)

2.1 `summary_pct_unwt` - unweighted

3\. `pa_summary_md` - median for Physical Activity module

4\. `tbls_mn_summary` - calculate means (including both parts: age range rows & total row)

5\. `tbls_pct_summary` - calculate percentages (inc. both parts: age range rows & total row)

6\. `fs_summary` - create a Fact Sheet summary

7\. `sort_rename_vars` - pivot the table (for percentages with more than two answer choices) and rename columns for standardization to produce the output used in the Data Book

8\. `tbls_list_split` - split the original list (by sex, urban/rural, region) from the summary function (tbls_mn_summary or tbls_pct_summary) into smaller lists for joining later with the join functions in the Data Book

9\. `forestplot_steps` - create individual forest plots (using ggforestplot package)

### R Markdown Functions

The `databook_functions.R` script (located in DataBook folder) contains functions used in RMDs for creating Word documents.

The list of functions:

`unite_ci` - unite low and upper confidence intervals into one CI variable

`apply_hux` - create hux tables (using huxtable package)

`final_m_w_b_tbl` - output clean-looking main tables (for men, women, both sexes)

`final_u_r_tbl` - output clean-looking urban/rural tables

`join_m_w_b` - join individual hux tables into one (men, women, both sexes)

`join_u_r` - join individual hux tables into one (urban, rural disaggregation)
