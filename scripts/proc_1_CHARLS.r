# <-- encoding UTF-8 -->
# Empirical study on CHARLS dataset (county level)
# -------------------------------------
## DOC STRING
# 
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------



rm(list=ls())


# -------------------------------------
## SECTION 0: ENVIRONMENT
library(readxl)  # xlsx I/O
library(car)  # VIF
library(sqldf)  # sql api
library(plm)  # panel data models
library(openxlsx)  # easy xlsx IO
library(psych)  # easiser PCA
source("./src/mathtools.r")
# library(plyr)  # sql-like dataframe operations


# environment pars
envCHARLS <- list(
    MicroDat = "./data/CHARLS_CITY.xlsx",  # CHARLS dataset (agent level)
    Output = "./output/proc_1_CHARLS/",  # output directory
    # ----------------------
    Ynames = c( "OUTP1M_RATIO", "CHRONIC_RATIO" ),  # dependents
    Xcore = c( "AVGINDIINCOME_EARN", "AVGEDU" ),  # the two variables we care most
    # ----------------------
    Xnames = c( "" ),  # a name list of selected independents (to regress), filled later
    Xcandidate = c( "" ),  # a name list of potential independents, filled later
    Xcontrol = c(""),  # selected control variables (not includes Xcore)
    Xexpel = c(
        "NONAGRIHUKOU_RATIO",
        # ------------------ outcomes
        "AVGCHRONIC_NUM", "HOSP1Y_RATIO", "AVGHOSP1Y_TIMES", "AVGOUTP1M_TIMES",  "AVGHOSPNOX_LAST",
        # ----------------- verbose
        # "AVGOUTP1M_REALEXP",
        "INSOTH_RATIO",
        "RETIRE_RATIO", "RETIREFORMAL_RATIO", "JOBSTATUS_RETI_RATIO",
        # ------------- asset
        "AVGINDIASSET_CASHSAVE" ,
        "AVGINDIASSET_CAPITAL" ,
        "AVGINDIASSET_GOVBOND" ,
        "AVGINDIASSET_OTHER" ,
        "AVGINDIASSET_TOTAL",
        # ----------------
        "AVGHOUSASSET_TOTAL",
        
        
        # "AVGINDIINCOME_EARN" ,
        # ------------- income
        "AVGHOUSINCOME_CAPITAL" ,
        "AVGHOUSINCOME_TOTAL",
        # ------------- income
        "AVGINDIINCOME_TOTAL",
        "AVGINDIINCOME_SELF" ,
        "AVGINDIINCOME_PENSION" ,
        "AVGINDIINCOME_TRANS" ,
        "AVGINDIINCOME_OTHER" ,
        "AVGINDIINCOME_FAMILY" ,
        "AVGTRANSCHILD_AMT",
        # ------------- transfer payment
        "AVGTRANSPAY_TOTAL"
        
        ),  # variables to drop, usually those almost-singular, or endogeneity etc
    # ----------------------
    flagCity = "city",  # individual flag (county level)
    flagProv = "province",  # individual flag (province level)
    flagT = "year",  # time flag
    flagSampleSize = "SAMPLESIZE",   # sample size of each city, used later to weight plotting data
    # ----------------------
    FigWidth = c( 12, 12, 12 ),  # width candidates for plots: 4:3, 16:9, 1:1
    FigHeight = c( 9, 27/4, 12 )  # heigth candidates
)







# -------------------------------------------------
# SECTION 1:  DATA PROCESSING
# NOTE: in this section, we read in the city-level aggregated dataset of harmonized CHARLS data (2011,2013,2015).
#       because R is quite low-efficient in processing large dataset like CHARLS,
#       we did this job in SAS and just import the aggregated dataset in this section.
#       if you need more information about how we did the data-processing job,
#       please refer to README.md;
#       I also attached the SAS program, two community-city-province index datasets, 
#       the codebook of the harmonized CHARLS data, and a detailed data dictionary in "./data/CHARLSdat/".
#       you may just run the "H_CHARLS_Clean.sas", when downloaded the original dataset from CHARLS's website. (too large, not attached)
#       but pls note: because Stata13 data is incompatible to ANGTHING but Stata13 itself, 
#       I had to convert the very original CAHRLS dataset to SPSS data (.sav) then read it into SAS. (be easy, no data, tags or labels lost)
#       please do so if you want to re-process our data aggregation process by yourself.
#       other data format, such as .xlsx, .csv may lose format information which records tags & encoding information.
#       and, a direct converting from .dta to .sas7bdat may also lose some information (e.g. variable name may change), if using improper converting tools.
# 
# Tianhao Zhao
# Dec 2018
# ------------
# 1.0 read in city-level CHARLS dataset
df_CHARLS_backup <- readxl::read_xlsx( envCHARLS$MicroDat )
names(df_CHARLS_backup)[names(df_CHARLS_backup) == "YEAR"] <- "year"  # for convenient coding later
# 1.1 a copy of original dataset, work on it
df_CHARLS <- df_CHARLS_backup
# 1.2 save the namelist of candidate control variables
tmp <- base::setdiff(names(df_CHARLS),envCHARLS$Ynames)
tmp <- base::setdiff(tmp,envCHARLS$Xcore)
tmp <- base::setdiff(tmp,envCHARLS$flagT)
tmp <- base::setdiff(tmp,envCHARLS$flagProv)
tmp <- base::setdiff(tmp,envCHARLS$flagCity)
tmp <- base::setdiff(tmp,envCHARLS$flagSampleSize)
tmp <- base::setdiff(tmp,envCHARLS$Xexpel)
envCHARLS$Xcandidate <- tmp
# 1.3 print info
cat("The number of candidate X: ",length(envCHARLS$Xcandidate), "\n\n" )
cat("They are: ", envCHARLS$Xcandidate, "\n\n")
# 1.4 re-sort the columns
df_CHARLS <- df_CHARLS[,c( envCHARLS$flagSampleSize, 
                           envCHARLS$flagCity, envCHARLS$flagT, envCHARLS$flagProv, 
                           envCHARLS$Ynames, envCHARLS$Xcore, envCHARLS$Xcandidate )]
# 1.5 drop NA obs
df_CHARLS <- na.omit(df_CHARLS)

















# ---------------------------------------
# SECTION 2: solve the problem of collinearty between the two core vars (income & edu)
cat("\nSection 2: COLLINEARITY BETWEEN INCOME & EDU\n-----------------------------------------------\n")
# NOTE: in CHARLS, agents are over 45 years old, they had finished education when interviewed,
#       and were receiving working inocmes or retired. 
#       both working & retirement have incomes, e.g. salary when working and pension benefites when retired.
#       Therefore, edu may affect income, but income cannot affect edu.
#       there is collinearity between the two, but the channel is one-way & clear (edu -> income).
#       Thus, We use edu to regress income, then use the residuals to replace income.
#       Then, the collinearity is solved, and we can clearly discuss the "pure" effect of income & edu
# NOTE: different from CHARLS, we have multiple candidates about incomes now. 
#       but we care the INDIVIDUAL TOTAL incomes, and drop other kinds of incomes
#       i.e. we, for now, do not distinguish different kinds of income sources
# ------------

# # 2.1 the correlation coef between income & edu:
# cat("Corr(AVGINDIINCOME_EARN,AVGEDU) = ", cor(df_CHARLS[,envCHARLS$Xcore] ), "\n" )
# # 2.2 regression
# tmp <- lm( AVGINDIINCOME_EARN ~ AVGEDU, df_CHARLS )
# # 2.3 replace income with the residuals
# df_CHARLS$AVGINDIINCOME_EARN <- residuals(tmp)
# # 2.4 print info
# cat("Corr(resid(AVGINDIINCOME_EARN ~ AVGEDU), AVGEDU) = ", cor(df_CHARLS[,envCHARLS$Xcore] ), "\nPossible Collinearity Solved\n" )
# 









# ---------------------------------------
# SECTION 3: select background/control variables through single-variable regression
cat("\nSection 3: SELECT BACKGROUND/CONTROL VARIABLES\n-----------------------------------------------\n")
# NOTE: We use alpha = 0.05;
# --------------
# 3.1 prepare a df to save significance of single-variable regressions
df_SignifSingle <- data.frame(
    IndicatorName = base::setdiff(envCHARLS$Xcandidate,envCHARLS$Xcore),  # we regress all indicators other than the core variables (income & edu)
    OUTP1M_RATIO = Inf,  # the p-val of the current indicator in model (illnessratio ~ 1 + CurrentIndicator)
    CHRONIC_RATIO = Inf # the same
)

# 3.2 do single-variable regressions, fill the df
for(tmpYname in envCHARLS$Ynames ){
    for(tmpXname in df_SignifSingle$IndicatorName ){
        # 3.2.1 regression
        eval(parse(text=paste(sep="",
                              "tmp <- summary(lm(", tmpYname, "~", tmpXname, ", data = df_CHARLS ))"
        )))
        # 3.2.2 get p-val
        eval(parse(text=paste(sep="",
                              "df_SignifSingle[df_SignifSingle$IndicatorName == \"", tmpXname, "\",\"", tmpYname, "\"] <- tmp$coefficients[2,4]"  # extract p-value 
        )))
    }    
}

# 3.3 check significance
df_SignifSingle[,envCHARLS$Ynames] <- df_SignifSingle[,envCHARLS$Ynames] < 0.05  # alpha = 0.05
df_SignifSingle$FlagSelected <- apply(df_SignifSingle[,envCHARLS$Ynames], 1, sum) > 0  # if significant on, at least, one health outcome indicator, then marked as selected (TRUE)

# 3.4 update envCHARLS.Xcontrol (no envCHARLS.Xcore, only selected control vars kept)
envCHARLS$Xcontrol <- as.character(df_SignifSingle$IndicatorName[df_SignifSingle$FlagSelected])

# 3.5 print info
cat("Drop these control variables through single-variable regressions:\n",
    setdiff(df_SignifSingle$IndicatorName, envCHARLS$Xcontrol ), "\n" )
cat("because they show no statistical significance on any health outcome indicators.\n")
cat("these control variables remain:\n",envCHARLS$Xcontrol,"\n")

# 3.6 recycling df_SignifSingle, delete dropped variables/rows
# NOTE: now it is used to save if a variable is selected as a control variable in the AIC-selected specification of each health outcome
#       (each column marks which control vars are selected by AIC criteria);
#       Meanwhile, envCHARLS.Xcores (incoem & edu) are forcely kept in the final specification;
df_SignifSingle$FlagSelected <- NULL  # not needed anymore, if kept, may be misleading
df_SignifSingle <- df_SignifSingle[df_SignifSingle$IndicatorName %in% envCHARLS$Xcontrol,  ]  # drop those dorpped vars
















# ---------------------------------------
# SECTION 3: MODEL SELECTION: PCA
# -------
# 3.1 a list to save PCA results generated by prcomp()
li_PCAres_CHARLS <- list(
    OUTP1M_RATIO = 0,
    CHRONIC_RATIO = 0
)
# 3.2 PCA analysis
# a vector to set max k components for each dependent
li_PCAk_CHARLS <- list(
    OUTP1M_RATIO = 4,
    CHRONIC_RATIO = 4
)
# 3.2.1 run
for(tmpYname in envCHARLS$Ynames){
    # estimate using psych library
    tmp <- as.character(df_SignifSingle$IndicatorName[ df_SignifSingle[,tmpYname] ] )  # index
    tmp <- psych::principal( df_CHARLS[,tmp], nfactors = li_PCAk_CHARLS[[tmpYname]], rotate = "varimax", scores = TRUE )  # rotate
    li_PCAres_CHARLS[[tmpYname]] <- tmp
        # # estimate (using build-in function prcomp() )
        # tmp <- as.character(df_SignifSingle$IndicatorName[ df_SignifSingle[,tmpYname] ] )  # index
        # li_PCAres_CHARLS[[tmpYname]] <- prcomp( df_CHARLS[,tmp], center = TRUE, scale. = TRUE, retx = TRUE, rank. = li_PCAk_CHARLS[[tmpYname]]  )
        # varmax()  # rotating
    # print
    cat("Dependent: ",tmpYname,"\n")
    print(  li_PCAres_CHARLS[[tmpYname]]  )
    cat("\n----------------------\n")
}
# 3.2.2 output (openxlsx can make every element in a list as a sheet in excel)
tmp <- list()  # eigen values
tmp1 <- list()  # loading matrix
for(tmpYname in envCHARLS$Ynames){
    tmp[[tmpYname]] <- li_PCAres_CHARLS[[tmpYname]]$values   # output eigen values
    tmp1[[tmpYname]] <- func_Load2Mat(  li_PCAres_CHARLS[[tmpYname]]$loadings  )  # loading matrix
}
openxlsx::write.xlsx(  tmp  , paste(sep="",envCHARLS$Output,"PCAresult_CHARLS.xlsx"),  rowNames = TRUE  )
openxlsx::write.xlsx(  tmp1  , paste(sep="",envCHARLS$Output,"PCAloading_CHARLS.xlsx"),  rowNames = TRUE  )






# 3.2.3 scree plots and output
# NOTE: for convenience, we plot it in excel! :)










# 3.2 construct a list to save the namelists of independents (Xcore + Pca) for every specification
li_Xnames_CHARLS <- list()
for(tmpYname in envCHARLS$Ynames){
    li_Xnames_CHARLS[[tmpYname]] <- c( envCHARLS$Xcore, paste(sep="","RC", 1:li_PCAk_CHARLS[[tmpYname]]  )  )
}









# 3.3 construct final specifications
li_Eq_CHARLS <- list()
for(tmpYname in envCHARLS$Ynames){
    # final specifications: y ~ income + edu + PC1 + ...
    li_Eq_CHARLS[[tmpYname]] <- func_GetEq( tmpYname,  li_Xnames_CHARLS[[tmpYname]]  ,
                                          Intercept = TRUE, KeepStr = FALSE
    )
    # print final specifications
    print(li_Eq_CHARLS[[tmpYname]]); cat("------------------------\n")
}






# 3.4 construct corresponding datasets
li_Dat_CHARLS <- list()  # general dataset
li_DatPlm_CHARLS <- list()  # plm indexed dataset
for(tmpYname in envCHARLS$Ynames){
    # extract principle components of current Y
    tmp <- li_PCAres_CHARLS[[tmpYname]]$scores
    # merge it with Y, Xcore, tags (indi, time)
    li_Dat_CHARLS[[tmpYname]] <- data.frame( df_CHARLS[,c( envCHARLS$flagT, envCHARLS$flagCity, tmpYname, envCHARLS$Xcore )] , tmp )
    li_DatPlm_CHARLS[[tmpYname]] <- plm::pdata.frame( li_Dat_CHARLS[[tmpYname]], index = c( envCHARLS$flagCity, envCHARLS$flagT ) )
}








# ---------------------------------------
# SECTION 4: VIF computing
cat("\nSection 4: VIF OF FINAL SPECIFICATIONS\n-----------------------------------------------\n")
# NOTE: up to now, we have finished the variable selection for CHARLS dataset;
#       the results are saved in li_PCAk_CHARLS, li_PCAres_CHARLS, li_Eq_CHARLS etc;
#       then we compute VIF for each specification and print:
cat("up to now, we have finished the variable selection for CHARLS dataset.")
cat("then we compute VIF for each specification and print:\n")
# -------
li_VIF_CHARLS <- list()  # a list to save VIF results
for(tmpYname in envCHARLS$Ynames){
    # get temp dataset
    tmp <- li_Dat_CHARLS[[tmpYname]]
    # compute VIF
    li_VIF_CHARLS[[tmpYname]] <- car::vif( lm( li_Eq_CHARLS[[tmpYname]], tmp ) )
    # print
    print(li_VIF_CHARLS[[tmpYname]]); cat("----------------------------\n")
}
# output to xlsx
openxlsx::write.xlsx( li_VIF_CHARLS, paste(sep="",envCHARLS$Output,"VIF_CHARLS.xlsx") )





# --------------------------------
# SECTION 6: GARBAGE COLLECTION
rm( tmp, tmpXname, tmpYname, df_SignifSingle )










