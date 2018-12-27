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
library(car)  # qqplots
library(plm)  # panel data models & Hausman test
library(nlme)  # gls estimation
library(nortest)  # normality tests
library(openxlsx)  # easy xlsx IO
source("./src/plottools.r")  # easy plotting tools
source("./src/paneltools.r")  # PANEL TOOLS

# environment par dict
envCHARLS$Output <- "./output/proc_3_CHARLS/"  # alter output directory




# -------------------------------------
## SECTION 1: (ROBUST) POOL DATA MODEL WITH FGLS & OLS
cat("\nSECTION 1: (ROBUST) POOL DATA MODELS ON CHARLS DATASET, ALL FINAL SPECIFICATIONS")
# NOTE: in this section, we seen the panel data set "dfp_CHARLS" as a pool dataset,
#       use OLS & GLS to estimate every final specification in li_Eq_CHARLS;
# 
# NOTE: using li_Dat_CHARLS (the fixed-effect mods' datasets) + random-effect mods' formulas (no mani-added effect terms)
# 
# NOTE: if extra normality tests on residuals required, please use the library nortest:: & shapiro.test()

# 1.0 prepare a collection of estimated models
li_ModPoolOLS_CHARLS <- list()
li_ModPoolGLS_CHARLS <- list()

# 1.1 estimation & print summaries
for(tmpYname in envCHARLS$Ynames){
    # slice a temp dataset
    tmpdf <- li_Dat_CHARLS[[tmpYname]]  # fixed-effect mods datasets!
    # 1.1.1 estimation
    li_ModPoolOLS_CHARLS[[tmpYname]] <- lm( li_EqRan_CHARLS[[tmpYname]], tmpdf )  # rand-effect mods formula!
    li_ModPoolGLS_CHARLS[[tmpYname]] <- nlme::gls( li_EqRan_CHARLS[[tmpYname]], tmpdf )
    # 1.1.2 run-time log
    cat("\n---------------------------------------------------------------\n***** FORMULA *****:\n")
    print( li_EqRan_CHARLS[[tmpYname]] ); print("\n")
    # 1.1.3 print summary
    print( summary( li_ModPoolOLS_CHARLS[[tmpYname]] ) )
    print( summary( li_ModPoolGLS_CHARLS[[tmpYname]] ) )
    
}





# --------------------------------------------
# SECTION: FIXED INDIVIDUAL EFFECT
# NOTE: because we use PROVINCE as individuals (but every obs is a city!),
#       we manually constructed the designed matrix then do regressions
#       quite diff from random effect models which are directly estimated by plm package,
#       these fixed-effect mods are estimated by ourselves
# -------------------
li_ModFix_CHARLS <- list()  # for fixed individual effect models
for(tmpYname in envCHARLS$Ynames){
    # slice a temp dataset
    tmpdf <- li_Dat_CHARLS[[tmpYname]]
    
    # 2.1.1 fixed effect model (FGLS estimator)
    li_ModFix_CHARLS[[tmpYname]] <- gls( li_EqFix_CHARLS[[tmpYname]], data = tmpdf  )
                                              
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("FIXED INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(   summary(  li_ModFix_CHARLS[[tmpYname]]   )   )
}












# -------------------------------------
## SECTION: RANDOM INDIVIDUAL EFFECT MODEL 
cat("\nSECTION: RANDOM INDIVIDUAL EFFECT MODEL")
# --------------
# 2.0 prepare collections
li_ModRan_CHARLS <- list()  # for random individual effect models


# 2.1 loop on health outcomes, estimate & perform Haussman test
for(tmpYname in envCHARLS$Ynames){
    # slice a temp dataset
    tmpdf <- li_DatPlm_CHARLS[[tmpYname]]

    # 2.1.2 random effect model (FGLS estimator)
    li_ModRan_CHARLS[[tmpYname]] <- plm::pggls( li_EqRan_CHARLS[[tmpYname]], data = tmpdf, 
                                              effect = "individual", model = "random" )
    
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("RANDOM INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(   summary(  li_ModRan_CHARLS[[tmpYname]]   )   )
}






# -------------------------------------
## SECTION 3: NORMALITY OF THE BENCHMARK MODEL (FIXED INDIVIDUAL EFFECT MODEL)
cat("\nSECTION 3: NOTMALITY OF THE BENCHMARK MODEL (FIXED INDIVIDUAL EFFECT MODEL)")
#       H0: normal distribution
# ---------------
df_NormTest_CHARLS <- list(
    Test = c( "Shapiro-Wilk", "Pearson Chi-square", "Lilliefor",
              "Anderson-Darling", "Cramer-von Mises" )  # test names
)  # saves p-values; converted to a df later

for(tmpYname in envCHARLS$Ynames){
    # 3.1 qqplots: plotting & output
    pdf(file = paste(sep="",envCHARLS$Output,"ResidQQ_FixIndi_CHARLS_",tmpYname,".pdf"), width = 12, height = 10)
    func_easyQQnorm( residuals(li_ModFix_CHARLS[[tmpYname]]), ylab = paste(sep="","resid(",tmpYname,")"), alpha = 0.05 )
    dev.off()
    # 3.2 normality tests
    tmp <- residuals(li_ModFix_CHARLS[[tmpYname]])
    df_NormTest_CHARLS[[tmpYname]] <- c(
        shapiro.test( tmp )$p.value,  # Shapiro-Wilk test
        nortest::pearson.test( tmp )$p.value,  # Peason chi-sqaure test
        nortest::lillie.test( tmp )$p.value,  # Kolmogorov-Smirnov test
        nortest::ad.test( tmp )$p.value,  # Anderson-Darling test
        nortest::cvm.test( tmp )$p.value  # Cramer-von Mises test
    )
}

# 3.3 convert to a dataframe then output
df_NormTest_CHARLS <- data.frame(df_NormTest_CHARLS)
write.csv(df_NormTest_CHARLS,file = paste(sep="",envCHARLS$Output,"NormTest_CHARLS.csv"))





# -------------------------------------
## SECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL
# NOTE: now, we manually construct datasets!
cat("\nSECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL")
# --------------
li_EqTwo_CHARLS <- list()
li_ModTwo_CHARLS <- list()

# 4.1 estimate & Haussman
for(tmpYname in envCHARLS$Ynames){
    # slice a temp data
    tmpdf <- li_Dat_CHARLS[[tmpYname]]
    # construct fixed time effect
    tmpdf <- func_MatDesignFixEffect( tmpdf, IndiName = envCHARLS$flagT )
    # construct new equation
    tmpxs <- base::setdiff( names(tmpdf), envCHARLS$Ynames  )
    li_EqTwo_CHARLS[[tmpYname]] <- func_GetEq( tmpYname, tmpxs, KeepStr = FALSE )
        
    # 4.1.1 estimate a two-way fixed effect model
    li_ModTwo_CHARLS[[tmpYname]] <- nlme::gls( li_EqTwo_CHARLS[[tmpYname]], tmpdf  )
    
    # print the two-way model
    print( summary( li_ModTwo_CHARLS[[tmpYname]]  ) )  # summary
    
}







# -------------------------------------
## SECTION 6: FORMATTED OUTPUT (FOR MS WORD)
# NOTE: in this section, we formatted the results for MS WORD software,
#       then output as excel file (because .csv may lead strings to be recognized as numbers )
# -----------
li_Report_CHARLS <- list()  # a list to save formatted reports
for(tmpYname in envCHARLS$Ynames){
    # 6.1 temp df
    tmpdf1 <- format_gls( summary(li_ModFix_CHARLS[[tmpYname]]  ) ,
                          Yname = paste(sep="",tmpYname,"_FixIndi_FGLS") )  # fixed  individual effect, OLS estimator + robustSE
    tmpdf2 <- format_pggls( summary(li_ModRan_CHARLS[[tmpYname]]) ,
                            Yname = paste(sep="",tmpYname,"_RanIndi_FGLS") )  # random individual effect, FGLS estimator
    tmpdf3 <- format_gls(   summary(li_ModTwo_CHARLS[[tmpYname]]    ) , 
                            Yname = paste(sep="",tmpYname,"_TwoWays_FGLS") )  # two-ways fixed effect model + robustSE
    tmpdf4 <- format_gls(   summary(li_ModPoolGLS_CHARLS[[tmpYname]])  ,
                            Yname = paste(sep="",tmpYname,"_Pooling_FGLS") )  # Pooling, GLS estimator
    tmpdf5 <- format_lm(    summary(li_ModPoolOLS_CHARLS[[tmpYname]])  ,
                            Yname = paste(sep="",tmpYname,"_Pooling_OLS") )  # Pooling, OLS estimator
    # 6.2 merging
    li_Report_CHARLS[[tmpYname]] <- plyr::join(
        tmpdf1, tmpdf2,
        by = "Variable", type = "full"
    )
    li_Report_CHARLS[[tmpYname]] <- plyr::join(
        li_Report_CHARLS[[tmpYname]], tmpdf3,
        by = "Variable", type = "full"
    )
    li_Report_CHARLS[[tmpYname]] <- plyr::join(
        li_Report_CHARLS[[tmpYname]], tmpdf4,
        by = "Variable", type = "full"
    )
    li_Report_CHARLS[[tmpYname]] <- plyr::join(
        li_Report_CHARLS[[tmpYname]], tmpdf5,
        by = "Variable", type = "full"
    )
    
}
# 6.3 garbage collection
rm( tmpdf1, tmpdf2, tmpdf3, tmpdf4, tmpdf5 )

# print info
print(li_Report_CHARLS)

# output to excel
openxlsx::write.xlsx(li_Report_CHARLS, file = paste(sep="",envCHARLS$Output,"RegReport_CHARLS.xlsx") )






