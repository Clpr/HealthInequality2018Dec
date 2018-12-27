# <-- encoding UTF-8 -->
# Empirical study on NHSS dataset (county level)
# -------------------------------------
## DOC STRING
# 
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------

# rm(list=ls()) # clear

# -------------------------------------
## SECTION 0: ENVIRONMENT
library(readxl)  # xlsx I/O
library(car)  # VIF
library(plm)  # panel data models
library(openxlsx)  # easy xlsx
library(psych)  # easy PCA
library(nlme)  # GLS
source("./src/mathtools.r")
source("./src/paneltools.r")

# environment pars
# NOTE: Xcandidates are selected through economic analysis (pls refer to our paper), 
#       then, the potential variables are further selected to enter the final specification
envNHSS <- list(
    Filepath = "./data/NHSSdat.xlsx",  # data file directory (read in)
    Output = "./output/proc_1_NHSS/",  # output directory
    # ----------------------
    Ynames = c( "illnessratio", "illnessday", "chronicratio" ),  # dependents
    Xcandidate = c( "" ),  # a name list of potential independents, filled later
    Xcontrol = c(""),  # selected control variables (not includes Xcore)
    # ----------------------
    Xcore = c( "income", "income2", "edu" ),  # the two variables we care most, income2 is the quaratic term of income
    # ----------------------
    # in addition to flagCounty, flagProv, flagT, other variable names to exclude
    # NOTE: we also expelled those variables which were not collected in both 1993 & 1998 (NA in both the years) (structural losses)
    ExpelVar = c( "district", "family",  # assistant vars, not in regressions
                  # those lost in 1998
                  "waittime", "information", "explain","advice","facility","consult","complain","satisfied","examination","clinicunsatisfied","hospitalunsatisfied",
                  # those lost in 1998, 2003
                  "waittime1", "birthweight", "bedday" ), 
    # ----------------------
    Xisflag = c( "urban" ),  # marks which (potential) independents are flags (1/0)
    # ----------------------
    flagCounty = "province",  # individual flag (of FIXED effect terms)
    flagProv = "ID",  # individual flag (of RANDOM effect terms)
    flagLevel = "level",  # administration level, e.g. big city, middle city, type-I rural area etc
    flagT = "year",  # time flag
    flagSampleSize = "population",   # sample size (or proxy, e.g. population) of each city, used later to weight plotting data
    # -------------------
    FigWidth = c( 12, 12, 12 ),  # width candidates for plots: 4:3, 16:9, 1:1
    FigHeight = c( 9, 27/4, 12 )  # heigth candidates
)





# -------------------------------------
## SECTION 1: DATA READING
cat("\nSection 1: DATA READING\n-----------------------------------------------\n")
# NOTE: in this section, we read in the NHSS dataset:
#       1. drop all 1993 data (many variables are structural lost; not eligible for regression)
#       2. get a namelist of potential/candidate regressors, we then do variable selection in the next section


# 1.0 read in NHSS data (pre-processed)
df_NHSSraw <- read_xlsx( envNHSS$Filepath )

# convert income to thousand yuan (because its square may leads to exact singularity in regression)
df_NHSSraw$income <- df_NHSSraw$income / 1000
# construct a quadratic term of income
df_NHSSraw$income2 <- (df_NHSSraw$income)^2

# 1.1 drop all 1993 data
df_NHSSraw <- df_NHSSraw[df_NHSSraw[,envNHSS$flagT] != 1993, ]

# 1.2 all potential control variables, and save their names to the environment dict
tmp <- names(df_NHSSraw) # a temp var to store names of all variables
tmp <- base::setdiff( tmp, envNHSS$Ynames )  # drop Y names
tmp <- base::setdiff( tmp, envNHSS$Xcore )  # drop core X
tmp <- base::setdiff( tmp, c( envNHSS$flagCounty, envNHSS$flagLevel, envNHSS$flagProv, envNHSS$flagT) )  # drop flags
tmp <- base::setdiff( tmp, envNHSS$ExpelVar )  # drop irrelevent, info, assistant vars
tmp <- base::setdiff( tmp, envNHSS$flagSampleSize )  # sample size is not used as an independent variable  
envNHSS$Xcandidate <- tmp  # get potential regressors (namelist)
cat("The number of candidate control variables: ",length(envNHSS$Xcandidate), "\n\n" )  # print info
cat("They are: ", envNHSS$Xcandidate, "\n\n")

# 1.3 slice the raw dataset (flags, Y, potential X)
tmp <- c( envNHSS$flagSampleSize, envNHSS$flagCounty, envNHSS$flagLevel, envNHSS$flagProv, envNHSS$flagT, envNHSS$Ynames, envNHSS$Xcore, envNHSS$Xcandidate)
df_NHSS <- df_NHSSraw[,tmp]

# 1.4 drop all obs with NA
df_NHSS <- na.omit(df_NHSS)





# ---------------------------------------
# SECTION 2: solve the problem of collinearty between the two core vars (income & edu)
cat("\nSection 2: COLLINEARITY BETWEEN INCOME & EDU\n-----------------------------------------------\n")
# NOTE: in NHSS, agents are in the age range about 20~48, they had finished education when interviewed,
#       and were receiving working inocmes. Therefore, edu may affect income, but income cannot affect edu.
#       there is collinearity between the two, but the channel is one-way & clear.
#       Thus, We use edu to regress income, then use the residuals to replace income.
#       Then, the collinearity is solved, and we can clearly discuss the "pure" effect of income & edu

# 2.0 the correlation coef between income & edu:
cat("Corr(income,edu) = ", cor(df_NHSS$income, df_NHSS$edu), "\n" )
# 2.1 regression
tmp <- lm( income ~ edu, data = df_NHSS )
# 2.2 replace income with the residuals
df_NHSS$income <- residuals.lm(tmp)
cat("Corr(resid(income ~ edu), edu) = ", cor(df_NHSS$income, df_NHSS$edu), "\nCollinearity Solved\n" )
df_NHSS$income2 <- (df_NHSS$income)^2





# ---------------------------------------
# SECTION 3: select background/control variables through single-variable regression
cat("\nSection 3: SELECT BACKGROUND/CONTROL VARIABLES\n-----------------------------------------------\n")
# NOTE: in this section, we select control variables from Xcandidates (except income & edu).
#       According to definition, socioeconomic status are defined as a collection of many variables, though,
#       where we care income & edu most. Thus, except incoem & edu, other socioeconomic indicators are used as control variables.
#       however, we cannot clear define what exactly indicators should be included in the collection of "socioeconomic status",
#       Therefore, we select those indicators which show statistical significance on AT LEAST ONE health outcome indicators (prevelance, illness day, chornic)
#       Meanwhile, we use AIC criteria to further select control variables to improve the explaination of variance,
#       also to deal with collinearity.
#       We use alpha = 0.05;

# 3.1 prepare a df to save significance of single-variable regressions
df_SignifSingle <- data.frame(
    IndicatorName = base::setdiff(envNHSS$Xcandidate,envNHSS$Xcore),  # we regress all indicators other than the core variables (income & edu)
    illnessratio = Inf,  # the p-val of the current indicator in model (illnessratio ~ 1 + CurrentIndicator)
    illnessday = Inf, # the same
    chronicratio = Inf,  # the same
    FlagSelected = FALSE  # if the indicator is selected as a control variable
)

# 3.2 do single-variable regressions, fill the df
for(tmpYname in envNHSS$Ynames ){
    for(tmpXname in setdiff(envNHSS$Xcandidate,envNHSS$Xcore) ){
        # 3.2.1 regression
        eval(parse(text=paste(sep="",
            "tmp <- summary(lm(", tmpYname, "~", tmpXname, ", data = df_NHSS ))"
        )))
        # 3.2.2 get p-val
        eval(parse(text=paste(sep="",
           "df_SignifSingle[df_SignifSingle$IndicatorName == \"", tmpXname, "\",\"", tmpYname, "\"] <- tmp$coefficients[2,4]"  # extract p-value 
        )))
    }    
}

# 3.3 check significance
df_SignifSingle[,envNHSS$Ynames] <- df_SignifSingle[,envNHSS$Ynames] < 0.05  # alpha = 0.05
df_SignifSingle$FlagSelected <- apply(df_SignifSingle[,envNHSS$Ynames], 1, sum) > 0  # if significant on, at least, one health outcome indicator, then marked as selected (TRUE)

# 3.4 update envNHSS.Xcontrol (no envNHSS.Xcore, only selected control vars kept)
envNHSS$Xcontrol <- as.character(df_SignifSingle$IndicatorName[df_SignifSingle$FlagSelected])

# 3.5 print info
cat("Drop these control variables through single-variable regressions:\n",
    setdiff(df_SignifSingle$IndicatorName, envNHSS$Xcontrol ), "\n" )
cat("because they show no statistical significance on any health outcome indicators.\n")
cat("these control variables remain:\n",envNHSS$Xcontrol,"\n")

# 3.6 recycling df_SignifSingle, delete dropped variables/rows
# NOTE: now it is used to save if a variable is selected as a control variable in the AIC-selected specification of each health outcome
#       (each column marks which control vars are selected by AIC criteria);
#       Meanwhile, envNHSS.Xcores (incoem & edu) are forcely kept in the final specification;
df_SignifSingle$FlagSelected <- NULL  # not needed anymore, if kept, may be misleading
df_SignifSingle <- df_SignifSingle[df_SignifSingle$IndicatorName %in% envNHSS$Xcontrol,  ]  # drop those dorpped vars








# ---------------------------------------
# SECTION 3: MODEL SELECTION: PCA
# do PCA selection for every health outcome indicator, on core Xs & all selected control vars
# NOTE: please be cautious!!!!!!!!!!!!!!!!!! the same names e.g. PC1 for diff Y are NOT EQUAL!!!!!!!!!! JUST THE SAME NAME!!!!!!!
# NOTE: we use minimum number of components which firstly explained 80% accumulated variance
# 3.1 a list to save PCA results generated by prcomp()
li_PCAres_NHSS <- list(
    illnessratio = 0,
    illnessday = 0,
    chronicratio = 0
)
# 3.2 PCA analysis
    # a vector to set max k components for each dependent
    li_PCAk_NHSS <- list(
        illnessratio = 3,
        illnessday = 4,
        chronicratio = 4
    )
    # 3.2.1 run
    for(tmpYname in envNHSS$Ynames){
        # estimate + rotate (psych)
        tmp <- as.character(df_SignifSingle$IndicatorName[ df_SignifSingle[,tmpYname] ] )  # index
        tmp <- psych::principal( df_NHSS[,tmp], nfactors = li_PCAk_NHSS[[tmpYname]], rotate = "varimax", scores = TRUE )  # rotate
        li_PCAres_NHSS[[tmpYname]] <- tmp
            # # estimate (built-in prcomp() )
            # tmp <- as.character(df_SignifSingle$IndicatorName[ df_SignifSingle[,tmpYname] ] )  # index
            # li_PCAres_NHSS[[tmpYname]] <- prcomp( df_NHSS[,tmp], center = TRUE, scale. = TRUE, retx = TRUE, rank. = li_PCAk_NHSS[[tmpYname]]  )
            # varmax()
        # print
        cat("Dependent: ",tmpYname,"\n")
        print(  li_PCAres_NHSS[[tmpYname]]   ); cat("\n")
        print(  li_PCAres_NHSS[[tmpYname]]$values )
        cat("\n----------------------\n")
    }
# 3.2.2 output (openxlsx can make every element in a list as a sheet in excel)
tmp <- list()  # eigen values
tmp1 <- list()  # loading matrix
for(tmpYname in envNHSS$Ynames){
    tmp[[tmpYname]] <- li_PCAres_NHSS[[tmpYname]]$values   # output engin values
    
    tmp1[[tmpYname]] <- func_Load2Mat(  li_PCAres_NHSS[[tmpYname]]$loadings  )  # loading matrix
}
openxlsx::write.xlsx(  tmp  , paste(sep="",envNHSS$Output,"PCAresult_NHSS.xlsx"),  rowNames = TRUE  )
openxlsx::write.xlsx(  tmp1  , paste(sep="",envNHSS$Output,"PCAloading_NHSS.xlsx"),  rowNames = TRUE )
    
    
    
    
    


# ------------------
# 3.4 construct corresponding datasets
# NOTE: because we use province other than city as individual fixed effects now, we need to manually construct a panel data frame
li_Dat_NHSS <- list()  # dataset, for FIXED EFFECT MODELS
li_DatPlm_NHSS <- list()  # plm indexed dataset, for RANDOM EFFECT MDOELS & POOLING (NO EFFECT) MODELS
for(tmpYname in envNHSS$Ynames){
    # extract principle components of current Y
    tmp <- li_PCAres_NHSS[[tmpYname]]$scores
    
    # construct datasets for fixed effect models
    li_Dat_NHSS[[tmpYname]] <- data.frame( df_NHSS[,c( envNHSS$flagT, envNHSS$flagCounty, tmpYname, envNHSS$Xcore )] , tmp )
    li_Dat_NHSS[[tmpYname]] <- func_MatDesignFixEffect( li_Dat_NHSS[[tmpYname]], IndiName = envNHSS$flagCounty )
    
    # construct datasets for random effect models & pooling models
    li_DatPlm_NHSS[[tmpYname]] <- plm::pdata.frame( data.frame( df_NHSS[,c( envNHSS$flagT, envNHSS$flagProv, tmpYname, envNHSS$Xcore )] , tmp ),
                                                    index = c( envNHSS$flagProv, envNHSS$flagT ) )
}





    
    
    
# 3.2 construct a list to save the namelists of independents (Xcore + Pca) for every specification
li_XnamesFix_NHSS <- list()
li_XnamesRan_NHSS <- list()

for(tmpYname in envNHSS$Ynames){
    # for random effect mods
    li_XnamesRan_NHSS[[tmpYname]] <- c( envNHSS$Xcore, paste(sep="","RC", 1:li_PCAk_NHSS[[tmpYname]]  )  )
    # for fixed effect mods
    tmp <- c( envNHSS$Xcore, base::setdiff( names(li_Dat_NHSS[[tmpYname]]), envNHSS$Ynames  )    )
    tmp <- base::setdiff( tmp, envNHSS$flagT )
    li_XnamesFix_NHSS[[tmpYname]] <- tmp
}




# 3.3 construct final specifications
li_EqFix_NHSS <- list()  # for fixed effect mods
li_EqRan_NHSS <- list()  # for random effect mods
for(tmpYname in envNHSS$Ynames){
    # final specifications: y ~ income + edu + PC1 + ...
    li_EqFix_NHSS[[tmpYname]] <- func_GetEq( tmpYname,  li_XnamesFix_NHSS[[tmpYname]]  ,
                                               Intercept = TRUE, KeepStr = FALSE
    )
    li_EqRan_NHSS[[tmpYname]] <- func_GetEq( tmpYname,  li_XnamesRan_NHSS[[tmpYname]]  ,
                                               Intercept = TRUE, KeepStr = FALSE
    )
    # print final specifications
    print(li_EqFix_NHSS[[tmpYname]]);print(li_EqRan_NHSS[[tmpYname]]);  
    cat("------------------------\n")
}











# ---------------------------------------
# SECTION 4: VIF computing
cat("\nSection 4: VIF OF FINAL SPECIFICATIONS\n-----------------------------------------------\n")
# NOTE: up to now, we have finished the variable selection for NHSS dataset;
#       the results are saved in li_PCAk_NHSS, li_PCAres_NHSS, li_Eq_NHSS etc;
#       then we compute VIF for each specification and print:
# 
# NOTE: in fact, we only need to compute the VIF of random-effect specifications
# 

cat("up to now, we have finished the variable selection for NHSS dataset.")
cat("then we compute VIF for each specification and print:\n")
# -------
li_VIF_NHSS <- list()  # a list to save VIF results
for(tmpYname in envNHSS$Ynames){
    # get temp dataset
    tmp <- li_Dat_NHSS[[tmpYname]]
    # compute VIF
    li_VIF_NHSS[[tmpYname]] <- data.frame( car::vif( lm( li_EqRan_NHSS[[tmpYname]], tmp ) )  )
    # print
    print(li_VIF_NHSS[[tmpYname]]); cat("----------------------------\n")
}
# output to xlsx
openxlsx::write.xlsx( li_VIF_NHSS, paste(sep="",envNHSS$Output,"VIF_NHSS.xlsx"), rowNames = TRUE )






    
# --------------------------------
# SECTION 6: GARBAGE COLLECTION
rm( tmp, tmpXname, tmpYname, df_SignifSingle )


    
    
















# -------------------- TRASH CODE -----------------


# for(tmpYname in envNHSS$Ynames){
#     
#     # 3.7.1 build a formula, forcely keep income & edu
#     tmp <- formula(paste(
#         tmpYname,"~",paste(collapse="+",envNHSS$Xcore),"+",paste(collapse="+",envNHSS$Xcontrol)
#     ))  # a basic/complete specification to select
#     # 3.7.2 benchmark model by OLS, pool data model
#     tmp <- lm(tmp,data=df_NHSS)
#     # 3.7.3 selection with AIC, forward; get selected model specification
#     tmp <- summary(step(tmp, direction="forward", trace = 0 ))  # trace = 0 print no info to console
#     # 3.7.4 get stat-significant variable names
#     tmp <- tmp$coefficients
#     tmp <- rownames(tmp)[ tmp[,4] < 0.05 ]  # alpha = 0.05
#     # 3.7.5 mark in the df_SignifSingle, if stat-significant, mark as TRUE, else, mark as FALSE
#     tmp <- df_SignifSingle$IndicatorName %in% tmp  # a logic vector; marks if a control variable is selected by AIC
#     eval(parse(text = paste(sep="",
#         "df_SignifSingle$",tmpYname," <- tmp"
#     )))  # save
# }





# # 3.8 add Xcore (income & edu) to df_SignifSingle to get complete final specifications
# tmp <- data.frame(
#     IndicatorName = envNHSS$Xcore, # a equiv structure df as df_SignifSingle
#     illnessratio = TRUE,  # all Xcore must be included in the final specification
#     illnessday = TRUE, 
#     chronicratio = TRUE
# )
# df_SignifSingle <- rbind( tmp, df_SignifSingle )




# 
# 
# # 3.9 save specifications & print final specifications to console
# li_Eq_NHSS <- list()  # an empty list to save the formulas of final specifications
# cat("\nAfter further selection with AIC+forward (on pool data model), we got final specifications for each health outcomes: \n\n")
# for(tmpYname in envNHSS$Ynames){
#     # select names of final independents
#     eval(parse(text=paste(sep="",
#         "tmp <- df_SignifSingle$",tmpYname," == TRUE"
#     )))
#     # construct a formula
#     tmp <- formula(paste(
#         tmpYname,"~",paste(collapse = "+",df_SignifSingle$IndicatorName[tmp])
#     )) 
#     # save to the list
#     eval(parse(text=paste(sep="",
#        "li_Eq_NHSS$",tmpYname," <- tmp"
#     )))
# }
# print(li_Eq_NHSS)
# 
# # 3.10 drop those lines/control variables (excluded by AIC) from df_SignifSingle
# # NOTE: if all health outcomes do not select a specific control variable, it is then be dropped from the dataframe
# tmp <- apply( df_SignifSingle[,envNHSS$Ynames], 1, sum ) != 0
# df_SignifSingle <- df_SignifSingle[tmp,]
# 
# # 3.11 get a copy record, cosistent with other scripts
# df_FinalSpecif_NHSS <- df_SignifSingle
# 
# # 3.12 output final specifications
# write.csv(df_FinalSpecif_NHSS,file= paste(sep="",envNHSS$Output,"FinalSpecification_NHSS.csv") )
# 
# 
# 







# 
# 
# 
# # ---------------------------------------
# # SECTION 4: VIF computing
# cat("\nSection 4: VIF OF FINAL SPECIFICATIONS\n-----------------------------------------------\n")
# # NOTE: up to now, we have finished the variable selection for NHSS dataset;
# #       the results are saved in df_SignifSingle
# #       then we compute VIF for each specification and print:
# cat("up to now, we have finished the variable selection for NHSS dataset.")
# cat("the results are saved in df_SignifSingle.")
# cat("then we compute VIF for each specification and print:\n")
# 
# # 4.1 compute VIF for each specification
# df_VIF_NHSS <- df_SignifSingle  # a df to store VIF results
# for(tmpYname in envNHSS$Ynames){
#     # 4.1.1 get the namelist of the final specification's X 
#     eval(parse(text=paste(sep="",
#         "tmpXname <- as.character(df_SignifSingle$IndicatorName[df_SignifSingle$",tmpYname,"])"
#     )))
#     # 4.1.2 slice a temp X's dataset to compute VIF
#     tmp <- df_NHSS[,tmpXname]
#     # 4.1.3 compute VIF & save
#     eval(parse(text = paste(sep="",
#         "tmp <- car::vif(lm(li_Eq_NHSS$",tmpYname,", data=df_NHSS) )"
#     )))
#     # 4.1.4 save to df_VIF_NHSS
#     eval(parse(text = paste(sep="",
#        "df_VIF_NHSS$",tmpYname," <- NA"
#     )))  # first, refresh all values to NA
#     eval(parse(text = paste(sep="",
#         "df_VIF_NHSS$",tmpYname,"[df_VIF_NHSS$IndicatorName %in% names(tmp)] <- tmp"
#     )))
#     
# }
# 
# # 4.2 print the VIF table
# cat("\nDisplay the VIFs of final specifications:\n")
# print(df_VIF_NHSS)
# 
# # 4.3 output the VIF table
# write.csv(df_VIF_NHSS,file=paste(sep="",envNHSS$Output,"VIF_NHSS.csv")   )
# 
# 
# 
# 
# 
# # --------------------------------
# # SECTION 5: CONVERT TO PANEL DATA TYPE
# dfp_NHSS <- pdata.frame(df_NHSS, index = c( envNHSS$flagCounty, envNHSS$flagT ) )
# 
# 




# # --------------------------------
# # SECTION 6: GARBAGE COLLECTION
# rm( tmp, tmpXname, tmpYname, df_SignifSingle )
# 





