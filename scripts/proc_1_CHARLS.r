# <-- encoding UTF-8 -->
# Empirical study on CHARLS dataset (county level)
# -------------------------------------
## DOC STRING
# 
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------






# -------------------------------------
## SECTION 0: ENVIRONMENT
library(readxl)  # xlsx I/O
library(car)  # VIF
library(sqldf)  # sql api
library(plm)  # panel data models
# library(plyr)  # sql-like dataframe operations


# environment pars
envCHARLS <- list(
    MicroDat = "./data/CHARLS_CITY.xlsx",  # CHARLS dataset (agent level)
    Output = "./output/proc_1_CHARLS/",  # output directory
    # ----------------------
    Ynames = c( "OUTP1M_RATIO", "CHRONIC_RATIO" ),  # dependents
    Xcore = c( "AVGINDIINCOME_TOTAL", "AVGEDU" ),  # the two variables we care most
    # ----------------------
    Xnames = c( "" ),  # a name list of selected independents (to regress), filled later
    Xcandidate = c( "" ),  # a name list of potential independents, filled later
    Xcontrol = c(""),  # selected control variables (not includes Xcore)
    Xexpel = c(
        "NONAGRIHUKOU_RATIO"
        ),  # variables to drop, usually those almost-singular
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
# SECTION 1: PROCESSING, AGGREGATING CHARLS DATASET
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
# 1.0 read in CHARLS original agent-level dataset
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
df_CHARLS <- df_CHARLS[,c( envCHARLS$flagSampleSize, envCHARLS$flagT, envCHARLS$flagProv, envCHARLS$flagCity, envCHARLS$Ynames, envCHARLS$Xcore, envCHARLS$Xcandidate )]
# 1.5 drop NA obs
df_CHARLS <- na.omit(df_CHARLS)









# ---------------------------------------
# SECTION 2: solve the problem of collinearty between the two core vars (income & edu)
cat("\nSection 2: COLLINEARITY BETWEEN INCOME & EDU\n-----------------------------------------------\n")
# NOTE: in NHSS, agents are over 45 years old, they had finished education when interviewed,
#       and were receiving working inocmes or retired. 
#       both working & retirement have incomes, e.g. salary when working and pension benefites when retired.
#       Therefore, edu may affect income, but income cannot affect edu.
#       there is collinearity between the two, but the channel is one-way & clear (edu -> income).
#       Thus, We use edu to regress income, then use the residuals to replace income.
#       Then, the collinearity is solved, and we can clearly discuss the "pure" effect of income & edu
# NOTE: different from NHSS, we have multiple candidates about incomes now. 
#       but we care the INDIVIDUAL TOTAL incomes, and drop other kinds of incomes
#       i.e. we, for now, do not distinguish different kinds of income sources
# ------------
# 2.0 drop other income variables
tmp <- c(
    "AVGHOUSINCOME_TOTAL",
    # "AVGINDIINCOME_TOTAL",  # we use this one!
    "AVGHOUSINCOME_CAPITAL",
    "AVGINDIINCOME_EARN",
    "AVGINDIINCOME_SELF",
    "AVGINDIINCOME_PENSION",
    "AVGINDIINCOME_TRANS",
    "AVGINDIINCOME_OTHER",
    "AVGINDIINCOME_FAMILY"
)
envCHARLS$Xcandidate <- base::setdiff(envCHARLS$Xcandidate, tmp)
df_CHARLS[,tmp] <- NULL
# 2.1 the correlation coef between income & edu:
# cat("Corr(AVGINDIINCOME_TOTAL,AVGEDU) = ", cor(df_CHARLS$AVGINDIINCOME_TOTAL, df_CHARLS$AVGEDU), "\n" )
# # 2.2 regression
# tmp <- lm( AVGINDIINCOME_TOTAL ~ AVGEDU, df_CHARLS )
# # 2.3 replace income with the residuals
# df_CHARLS$AVGINDIINCOME_TOTAL <- residuals(tmp)
# # 2.4 print info
# cat("Corr(resid(AVGINDIINCOME_TOTAL ~ AVGEDU), AVGEDU) = ", cor(df_CHARLS$AVGINDIINCOME_TOTAL, df_CHARLS$AVGEDU), "\nCollinearity Solved\n" )







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

# 3.7 do AIC-forward selection for every health outcome indicator, on core Xs & all selected control vars
for(tmpYname in envCHARLS$Ynames){
    # 3.7.1 build a formula, forcely keep income & edu
    tmp <- formula(paste(
        tmpYname,"~",paste(collapse="+",envCHARLS$Xcore),"+",paste(collapse="+",envCHARLS$Xcontrol)
    ))  # a basic/complete specification to select
    # 3.7.2 benchmark model by OLS, pool data model
    tmp <- lm(tmp,data=df_CHARLS)
    # 3.7.3 selection with AIC, forward; get selected model specification
    tmp <- summary(step(tmp, direction="forward", trace = 0 ))  # trace = 0 print no info to console
    # 3.7.4 get stat-significant variable names
    tmp <- tmp$coefficients
    tmp <- rownames(tmp)[ tmp[,4] < 0.05 ]  # alpha = 0.05
    # 3.7.5 mark in the df_SignifSingle, if stat-significant, mark as TRUE, else, mark as FALSE
    tmp <- df_SignifSingle$IndicatorName %in% tmp  # a logic vector; marks if a control variable is selected by AIC
    eval(parse(text = paste(sep="",
                            "df_SignifSingle$",tmpYname," <- tmp"
    )))  # save
}

# 3.8 add Xcore (income & edu) to df_SignifSingle to get complete final specifications
tmp <- data.frame(
    IndicatorName = envCHARLS$Xcore, # a equiv structure df as df_SignifSingle
    OUTP1M_RATIO = TRUE,  # all Xcore must be included in the final specification
    CHRONIC_RATIO = TRUE
)
df_SignifSingle <- rbind( tmp, df_SignifSingle )

# 3.9 save specifications & print final specifications to console
li_Eq_CHARLS <- list()  # an empty list to save the formulas of final specifications
cat("\nAfter further selection with AIC+forward (on pool data model), we got final specifications for each health outcomes: \n\n")
for(tmpYname in envCHARLS$Ynames){
    # select names of final independents
    eval(parse(text=paste(sep="",
                          "tmp <- df_SignifSingle$",tmpYname," == TRUE"
    )))
    # construct a formula
    tmp <- formula(paste(
        tmpYname,"~",paste(collapse = "+",df_SignifSingle$IndicatorName[tmp])
    )) 
    # save to the list
    eval(parse(text=paste(sep="",
                          "li_Eq_CHARLS$",tmpYname," <- tmp"
    )))
}
print(li_Eq_CHARLS)

# 3.10 drop those lines/control variables (excluded by AIC) from df_SignifSingle
# NOTE: if all health outcomes do not select a specific control variable, it is then be dropped from the dataframe
tmp <- apply( df_SignifSingle[,envCHARLS$Ynames], 1, sum ) != 0
df_SignifSingle <- df_SignifSingle[tmp,]

# 3.11 get a copy record, cosistent with other scripts
df_FinalSpecif_CHARLS <- df_SignifSingle

# 3.12 output final specifications
write.csv(df_FinalSpecif_CHARLS,file= paste(sep="",envCHARLS$Output,"FinalSpecification_CHARLS.csv") )










# ---------------------------------------
# SECTION 4: VIF computing
cat("\nSection 4: VIF OF FINAL SPECIFICATIONS\n-----------------------------------------------\n")
# ----------------
# 4.1 compute VIF for each specification
df_VIF_CHARLS <- df_SignifSingle  # a df to store VIF results
for(tmpYname in envCHARLS$Ynames){
    # 4.1.1 get the namelist of the final specification's X 
    eval(parse(text=paste(sep="",
                          "tmpXname <- as.character(df_SignifSingle$IndicatorName[df_SignifSingle$",tmpYname,"])"
    )))
    # 4.1.2 slice a temp X's dataset to compute VIF
    tmp <- df_CHARLS[,tmpXname]
    # 4.1.3 compute VIF & save
    eval(parse(text = paste(sep="",
                            "tmp <- car::vif(lm(li_Eq_CHARLS$",tmpYname,", data=df_CHARLS) )"
    )))
    # 4.1.4 save to df_VIF_NHSS
    eval(parse(text = paste(sep="",
                            "df_VIF_CHARLS$",tmpYname," <- NA"
    )))  # first, refresh all values to NA
    eval(parse(text = paste(sep="",
                            "df_VIF_CHARLS$",tmpYname,"[df_VIF_CHARLS$IndicatorName %in% names(tmp)] <- tmp"
    )))
    
}
# 4.2 print the VIF table
cat("\nDisplay the VIFs of final specifications:\n")
print(df_VIF_CHARLS)

# 4.3 output the VIF table
write.csv(df_VIF_CHARLS,file=paste(sep="",envCHARLS$Output,"VIF_CHARLS.csv")   )




# --------------------------------
# SECTION 5: CONVERT TO PANEL DATA TYPE
dfp_CHARLS <- pdata.frame(df_CHARLS, index = c( envCHARLS$flagCity, envCHARLS$flagT ) )


# --------------------------------
# SECTION 6: GARBAGE COLLECTION
rm( tmp, tmpXname, tmpYname, df_SignifSingle )
















