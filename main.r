# <-- encoding UTF-8 -->
# Portal for the code of: Socioeconomic Status Inequality and Morbidity Rate Differences in China: Based on NHSS and CHARLS Data
# Author: Y. Jiang, H. Zheng, T. Zhao
# -------------------------------------------
## DOC STRING
# 
# The script is the portal of all programs of our submtted paper;
# you may run this file, or follow the documentation, README, and comments to specific source files.
# Please refer to README.md for more information.
# and, all scripts & file I/O are encoded in UTF-8 (because Chinese charaters, in data and/or scripts, may be included & used)
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------------
## DEPENDENCY
#
# 1. readxl:    IO of xlsx files
# 2. plm:       panel data models
# 3. nlme:      GLS estimation of linear models
# 4. car:       VIFs, qqplots
# 5. ggplot2, plyr, maptools, mapproj, Cairo: map drawing
# 6. sqldf:     api of firendly sql enqury
# 7. nortest:   normality tests
# 8. purrr:     std lib, to do function operations
# 9. openxlsx:  easy xlsx I/O, no dependency on rJava
# -------------------------------------------

rm(list=ls()) # clear
setwd("C:/Users/Lenovo/OneDrive/GitHubProj/HealthInequality2018Dec")  # working directory








# -------------------------------------------
# Section 1: NHSS data processing & variable selection
cat("\nScript 1: NHSS data processing & variable selection")
# MISSION:
#       1. NHSS data I/O
#       2. NHSS data processing, missing values
#       3. collinearity between income & edu: resid(income ~ edu) --> income
#       4. NHSS (control) variable selection I: through single-variable regressions
#       5. NHSS (control) variable selection II: AIC stepwise to FURTHER select control variables into final specifications
#       6. VIFs of final specifications
source("./scripts/proc_1_NHSS.r")
# LEGACY IN WORKSPACE:
#       1. df_NHSSraw:              raw NHSS dataset, consisting of all potential vars & useless vars; as backup
#       2. df_NHSS:                 processed NHSS dataset
#       3. dfp_NHSS:                a panel-data-type dataframe (converted from df_NHSS, used in package plm)
#       4. df_FinalSpecif_NHSS:     a dataframe marking which variables in the final specifications
#       5. df_VIF_NHSS:             VIF results of final specifications
#       6. li_Eq_NHSS:              a list consisting of formula objects of final specifications
#       7. envNHSS:                 environment variables of NHSS dataset
# NOTE: pls go to ./output/proc_1_NHSS/ for output results (if applicable)
# ---------------------------------------------

# ---------------------------------------------
# Section 2: NHSS data, descriptive statistics (basic and advanced)
cat("\nScript 2: NHSS data descriptive statistics")
# DEPENDENCY: using legacy of proc_1_NHSS.r
# MISSION:
#       1. NHSS data, general descriptive stats (mean, sd etc)
#       2. county-level (individual) Lorenz Curve & Gini coef of health outcomes
#       3. county-level (individual) Theil-I, Theil-II
#       4. county-level (individual) C.V., coefficient of variance
#       5. county-level (individual) Variance
#       6. NHSS data, the existence of the inequalities of health outcomes among areas in China (colored map)
#       7. NHSS data, the difference of (income & edu) among areas in china (colored map)
source("./scripts/proc_2_NHSS.r")
# LEGACY IN WORKSPACE:
#       1. df_Descript_NHSS:            a table of the descriptive statistics of NHSS data (final specification)
#       2. df_InequalIdx_NHSS:          a table of different kinds of inequality indices of NHSS data
#       3. MapShapeCH:                  a dataset of Chinese GIS data; will be used later
#       4. func_DescrStat:              a function to do descriptive statistics
#       5. func_MapProv:                draws colored map based on Chinese GIS data; province level
#       6. func_SaveMap2PDF:            easy output method of the figures created by func_MapProv()
#       7. LorenzCurve:                 easy function to compute Lorenz curve & Gini coef
#       8. Theil:                       easy function to compute Theil-I/II index
#       9. TrapeInt:                    easy function to compute trapezium integral
# NOTE: pls go to ./output/proc_2_NHSS/ for output results (applicable)
# ---------------------------------------------

# ---------------------------------------------
# Section 3: NHSS data, econometric analysis (pool, fix, random, Hausman)
cat("\nScript 3: NHSS data, econometric analysis (pool, fix, random, Hausman)")
# DEPENDENCY: using legacy of proc_1_NHSS.r
# 
# IMPORTANT NOTE: please refer to PanelAnalysisLogic.md under the ./docs directory to help understand how we designed this section !!! :)
# 
# MISSION:
#       1. one-way fixed individual effect model & random individual effect model (FGLS estimators)
#       2. Hausman test & robust Hausman test (Wooldridge, 2010)
#       3. Residual QQplots & normality tests, one-way fixed individual effect model (only)
#       4. ROBUST: two-ways fixed effect model & Haussman test (to see if one-way & two-ways are consistent)
#       5. ROBUST: pooling, OLS
#       6. ROBUST: pooling, FGLS
#        
#       5. ROBUST: add possible independent: 
source("./scripts/proc_3_NHSS.r")
# LEGACY IN WORKSPACE:
# NOTE: pls go to ./output/proc_3_NHSS/ for output results (applicable)
# -------------------------------------------











# -------------------------------------------
# Section 4: CHARLS data processing & variable selection
cat("\nScript 4: CHARLS data processing & variable selection")
# MISSION:
#       1. CHARLS data I/O
#       2. CHARLS data processing, missing values
#       3. collinearity between income & edu: resid(income ~ edu) --> income
#       4. CHARLS (control) variable selection I: through single-variable regressions
#       5. CHARLS (control) variable selection II: AIC stepwise to FURTHER select control variables into final specifications
#       6. VIFs of final specifications
source("./scripts/proc_1_CHARLS.r")
# LEGACY IN WORKSPACE:
#       1. df_CHARLS_backup:            raw CHARLS dataset, consisting of all potential vars & useless vars; as backup
#       2. df_CHARLS:                   processed CHARLS dataset
#       3. dfp_CHARLS:                  a panel-data-type dataframe (converted from df_NHSS, used in package plm)
#       4. df_FinalSpecif_CHARLS:       a dataframe marking which variables in the final specifications
#       5. df_VIF_CHARLS:               VIF results of final specifications
#       6. li_Eq_CHARLS:                a list consisting of formula objects of final specifications
#       7. envCHARLS:                   environment variables of CHARLS dataset
# NOTE: pls go to ./output/proc_1_CHARLS/ for output results (if applicable)
# ---------------------------------------------

# ---------------------------------------------
# Section 5: CHARLS data, descriptive statistics (basic and advanced)
cat("\nScript 5: CHARLS data, descriptive statistics (basic and advanced)")
# DEPENDENCY: using legacy of proc_1_NHSS.r
# MISSION:
#       1. CHARLS data, general descriptive stats (mean, sd etc)
#       2. county-level (individual) Lorenz Curve & Gini coef of health outcomes
#       3. county-level (individual) Theil-I, Theil-II
#       4. county-level (individual) C.V., coefficient of variance
#       5. county-level (individual) Variance
#       6. CHARLS data, the existence of the inequalities of health outcomes among areas in China (colored map)
#       7. CHARLS data, the difference of (income & edu) among areas in china (colored map)
source("./scripts/proc_2_CHARLS.r")
# LEGACY IN WORKSPACE:
#       1. df_Descript_CHARLS:      a table of the descriptive statistics of NHSS data (final specification)
#       2. df_InequalIdx_CHARLS:    a table of different kinds of inequality indices of NHSS data
#       3. MapShapeCH:              a dataset of Chinese GIS data; will be used later
#       4. func_DescrStat:          a function to do descriptive statistics
#       5. func_MapProv:            draws colored map based on Chinese GIS data; province level
#       6. func_SaveMap2PDF:        easy output method of the figures created by func_MapProv()
#       7. LorenzCurve:             easy function to compute Lorenz curve & Gini coef
#       8. Theil:                   easy function to compute Theil-I/II index
#       9. TrapeInt:                easy function to compute trapezium integral
# NOTE: pls go to ./output/proc_2_CHARLS/ for output results (applicable)
# ---------------------------------------------

# ---------------------------------------------
# Section 6: CHARLS data, econometric analysis (pool, fix, random, Hausman)
cat("\nScript 3: CHARLS data, econometric analysis (pool, fix, random, Hausman)")
# DEPENDENCY: using legacy of proc_1_CHARLS.r
# 
# IMPORTANT NOTE: please refer to PanelAnalysisLogic.md under the ./docs directory to help understand how we designed this section !!! :)
# 
# MISSION:
#       1. one-way fixed individual effect model & random individual effect model (FGLS estimators)
#       2. Hausman test & robust Hausman test (Wooldridge, 2010)
#       3. Residual QQplots & normality tests, one-way fixed individual effect model (only)
#       4. ROBUST: two-ways fixed effect model & Haussman test (to see if one-way & two-ways are consistent)
#       5. ROBUST: pooling, OLS
#       6. ROBUST: pooling, FGLS
#        
#       5. ROBUST: add possible independent: 
source("./scripts/proc_3_CHARLS.r")
# LEGACY IN WORKSPACE:
# NOTE: pls go to ./output/proc_3_CHARLS/ for output results (applicable)
# -------------------------------------------





















