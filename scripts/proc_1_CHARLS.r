# <-- encoding UTF-8 -->
# Empirical study on CHARLS dataset (county level)
# -------------------------------------
## DOC STRING
# 
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------



# rm(list=ls())


# -------------------------------------
## SECTION 0: ENVIRONMENT
library(readxl)  # xlsx I/O
library(car)  # VIF
library(sqldf)  # sql api
library(plm)  # panel data models
library(openxlsx)  # easy xlsx IO
library(psych)  # easiser PCA
library(nlme)  # GLS
source("./src/mathtools.r")
source("./src/paneltools.r")
# library(plyr)  # sql-like dataframe operations


# environment pars
envCHARLS <- list(
    MicroDat = "./data/CHARLS_CITY.xlsx",  # CHARLS dataset (agent level)
    Output = "./output/proc_1_CHARLS/",  # output directory
    # ----------------------
    Ynames = c( "OUTP1M_RATIO", "CHRONIC_RATIO" ),  # dependents
    Xcore = c( "AVGINDIINCOME_EARN", "AVGINDIINCOME_EARN2", "AVGEDU" ),  # the two variables we care most
    # ----------------------
    Xnames = c( "" ),  # a name list of selected independents (to regress), filled later
    Xcandidate = c( "" ),  # a name list of potential independents, filled later
    Xcontrol = c(""),  # selected control variables (not includes Xcore)
    Xexpel = c(
        # ------------------ duplicated
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
        # ---------------- asset
        "AVGHOUSASSET_TOTAL",
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
    flagCity = "province",  # individual flag (province level)  ps: well ... just a mark
    flagProv = "city",  # individual flag (city level)
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
#       if you need more information about how we did the data-processing,
#       please refer to README.md;
#       I also attached the SAS program, two community-city-province index datasets, 
#       the codebook of the harmonized CHARLS data, and a detailed data dictionary in "./data/CHARLSdat/".
#       you may just run the "H_CHARLS_Clean.sas", when downloaded the original dataset from CHARLS's website. (not attached)
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
# construct a sqaured term of AVGINDIINCOME_EARN
df_CHARLS_backup$AVGINDIINCOME_EARN2 <- (df_CHARLS_backup$AVGINDIINCOME_EARN)^2
# 1.1 a copy of original dataset, work on it
df_CHARLS <- df_CHARLS_backup

# # 1.2 save the namelist of candidate control variables
# tmp <- base::setdiff(names(df_CHARLS),envCHARLS$Ynames)
# tmp <- base::setdiff(tmp,envCHARLS$Xcore)
# tmp <- base::setdiff(tmp,envCHARLS$flagT)
# tmp <- base::setdiff(tmp,envCHARLS$flagProv)
# tmp <- base::setdiff(tmp,envCHARLS$flagCity)
# tmp <- base::setdiff(tmp,envCHARLS$flagSampleSize)
# tmp <- base::setdiff(tmp,envCHARLS$Xexpel)
envCHARLS$Xcandidate <- c(   # prepare to select & PCA
    "AVGAGE", "MALE_RATIO", "MARITAL_RATIO", "MARITAL_AVELEN",  "URBAN_RATIO",  
    "AVGBMI",  "DRINK1Y_RATIO", "SMOKEEVER_RATIO",  
    "SMOKENOW_RATIO",    "AVGSMOKENUM",   "AVGHOSP1Y_REALEXP", "AVGOUTP1M_REALEXP", "INSURANCE_RATIO",   "INSGOV_RATIO",  "INSPRI_RATIO",  "AVGEXP1W_FOOD",    
    "AVGEXP1Y_TOTAL",    "CHILDCARE_RATIO",   "CHILDCORESD_RATIO", "CHILDLVNEAR_RATIO", "SOCWK_RATIO",   "TRANSCHILD_RATIO",  "WORK_RATIO",    
    "JOBSTATUS_AGRI_RATIO", "JOBSTATUS_NAGE_RATIO", "JOBSTATUS_NAGS_RATIO", "JOBSTATUS_NAGF_RATIO", "JOBSTATUS_UNEM_RATIO", "JOBSTATUS_NEWK_RATIO"
)

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









# ---------------------------------------
# SECTION 3: select background/control variables through single-variable regression
cat("\nSection 3: SELECT BACKGROUND/CONTROL VARIABLES\n-----------------------------------------------\n")
# NOTE: We use alpha = 0.05;
# --------------
# 3.1 prepare a df to save significance of single-variable regressions
df_SignifSingle <- data.frame(
    IndicatorName = base::setdiff(envCHARLS$Xcandidate,envCHARLS$Xcore),  # we regress all indicators other than the core variables (income & edu)
    # OUTP1M_RATIO = Inf,  # the p-val of the current indicator in model (illnessratio ~ 1 + CurrentIndicator)
    CHRONIC_RATIO = Inf # the same
)

# 3.2 do single-variable regressions, fill the df
for(tmpYname in envCHARLS$Ynames ){
    for(tmpXname in df_SignifSingle$IndicatorName ){
        # 3.2.1 regression
        eval(parse(text=paste(sep="",
                              "tmp <- summary(gls(", tmpYname, "~", " AVGINDIINCOME_EARN + AVGEDU + " ,tmpXname, ", data = df_CHARLS ))"
        )))
        # 3.2.2 get p-val
        eval(parse(text=paste(sep="",
                              "df_SignifSingle[df_SignifSingle$IndicatorName == \"", tmpXname, "\",\"", tmpYname, "\"] <- tmp$tTable[\"",tmpXname,"\",4]"  # extract p-value 
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
    OUTP1M_RATIO = 6,
    CHRONIC_RATIO = 7
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
    print(  li_PCAres_CHARLS[[tmpYname]]  ); cat("\nEigen Values: ")
    print( li_PCAres_CHARLS[[tmpYname]]$values )
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





# ------------------------------- NOTE: different from NHSS! because we use province as individual now

# 3.4 construct corresponding datasets
li_Dat_CHARLS <- list()  # general dataset, for fixed effect models
li_DatPlm_CHARLS <- list()  # plm indexed dataset
for(tmpYname in envCHARLS$Ynames){
    # extract principle components of current Y
    tmp <- li_PCAres_CHARLS[[tmpYname]]$scores
    # merge it with Y, Xcore, tags (indi, time)
    # NOTE: we will MANUALLY estimate fixed effects
    li_Dat_CHARLS[[tmpYname]] <- data.frame( df_CHARLS[,c( envCHARLS$flagT, envCHARLS$flagCity, tmpYname, envCHARLS$Xcore )] , tmp ) # flagCity, in fact, just indicates what individual variable we use, not only "city"
    # parse individual tags to a designed dataframe
    li_Dat_CHARLS[[tmpYname]] <- func_MatDesignFixEffect( li_Dat_CHARLS[[tmpYname]], IndiName = envCHARLS$flagCity )
    # this version (pdata.frame), only used to estimate RANDOM EFFECT
    li_DatPlm_CHARLS[[tmpYname]] <- plm::pdata.frame(  data.frame( df_CHARLS[,c( envCHARLS$flagT, envCHARLS$flagProv, tmpYname, envCHARLS$Xcore )] , tmp )  ,
                                                       index = c( envCHARLS$flagProv, envCHARLS$flagT ) ) # look, we change flagCity to flagProv! (well, it is REAL city now ...)
}







# 3.2 construct a list to save the namelists of independents (Xcore + Pca) for every specification
li_XnamesFix_CHARLS <- list()  # for fixed effects
li_XnamesRan_CHARLS <- list()  # for random effects & pooling

for(tmpYname in envCHARLS$Ynames){
    # for random effect mods
    li_XnamesRan_CHARLS[[tmpYname]] <- c( envCHARLS$Xcore, paste(sep="","RC", 1:li_PCAk_CHARLS[[tmpYname]]  )  )
    # for fixed effect mods
    tmp <- c( envCHARLS$Xcore, base::setdiff( names(li_Dat_CHARLS[[tmpYname]]), envCHARLS$Ynames  )    )
    tmp <- base::setdiff( tmp, envCHARLS$flagT )
    li_XnamesFix_CHARLS[[tmpYname]] <- tmp
}





# 3.3 construct final specifications
li_EqFix_CHARLS <- list()  # for fixed effect mods
li_EqRan_CHARLS <- list()  # for random effect mods
for(tmpYname in envCHARLS$Ynames){
    # final specifications: y ~ income + edu + PC1 + ...
    li_EqFix_CHARLS[[tmpYname]] <- func_GetEq( tmpYname,  li_XnamesFix_CHARLS[[tmpYname]]  ,
                                          Intercept = TRUE, KeepStr = FALSE
    )
    li_EqRan_CHARLS[[tmpYname]] <- func_GetEq( tmpYname,  li_XnamesRan_CHARLS[[tmpYname]]  ,
                                               Intercept = TRUE, KeepStr = FALSE
    )
    # print final specifications
    print(li_EqFix_CHARLS[[tmpYname]]);print(li_EqRan_CHARLS[[tmpYname]]);  
    cat("------------------------\n")
}












# ---------------------------------------
# SECTION 4: VIF computing
cat("\nSection 4: VIF OF FINAL SPECIFICATIONS\n-----------------------------------------------\n")
# NOTE: up to now, we have finished the variable selection for CHARLS dataset;
#       the results are saved in li_PCAk_CHARLS, li_PCAres_CHARLS, li_Eq_CHARLS etc;
#       then we compute VIF for each specification and print:
# 
# NOTE: in fact, we only need to compute the VIF of random-effect specifications
# 

cat("up to now, we have finished the variable selection for CHARLS dataset.")
cat("then we compute VIF for each specification and print:\n")
# -------
li_VIF_CHARLS <- list()  # a list to save VIF results
for(tmpYname in envCHARLS$Ynames){
    # get temp dataset
    tmp <- li_Dat_CHARLS[[tmpYname]]
    # compute VIF
    li_VIF_CHARLS[[tmpYname]] <- data.frame( car::vif( lm( li_EqRan_CHARLS[[tmpYname]], tmp ) )  )
    # print
    print(li_VIF_CHARLS[[tmpYname]]); cat("----------------------------\n")
}
# output to xlsx
openxlsx::write.xlsx( li_VIF_CHARLS, paste(sep="",envCHARLS$Output,"VIF_CHARLS.xlsx"), rowNames = TRUE )





# --------------------------------
# SECTION 6: GARBAGE COLLECTION
rm( tmp, tmpXname, tmpYname, df_SignifSingle )










