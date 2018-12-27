# <-- encoding UTF-8 -->
# Empirical study on NHSS dataset (county level)
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
source("./src/paneltools.r")  # extra tools for plm package, e.g. formatted summary for MS WORD


# environment par dict
envNHSS$Output <- "./output/proc_3_NHSS/"  # alter output directory





# -------------------------------------
## SECTION 1: (ROBUST) POOL DATA MODEL WITH FGLS & OLS
cat("\nSECTION 1: (ROBUST) POOL DATA MODELS ON NHSS DATASET, ALL FINAL SPECIFICATIONS")
# NOTE: in this section, we seen the panel data set "dfp_NHSS" as a pool dataset,
#       use OLS & GLS to estimate every final specification in li_Eq_NHSS;
# 
# NOTE: using li_Dat_NHSS (the fixed-effect mods' datasets) + random-effect mods' formulas (no mani-added effect terms)
# 
# NOTE: if extra normality tests on residuals required, please use the library nortest:: & shapiro.test()

# 1.0 prepare a collection of estimated models
li_ModPoolOLS_NHSS <- list()
li_ModPoolGLS_NHSS <- list()

# 1.1 estimation & print summaries
for(tmpYname in envNHSS$Ynames){
    # slice a temp dataset
    tmpdf <- li_Dat_NHSS[[tmpYname]]  # fixed-effect mods datasets!
    # 1.1.1 estimation
    li_ModPoolOLS_NHSS[[tmpYname]] <- lm( li_EqRan_NHSS[[tmpYname]], tmpdf )  # rand-effect mods formula!
    li_ModPoolGLS_NHSS[[tmpYname]] <- nlme::gls( li_EqRan_NHSS[[tmpYname]], tmpdf )
    # 1.1.2 run-time log
    cat("\n---------------------------------------------------------------\n***** FORMULA *****:\n")
    print( li_EqRan_NHSS[[tmpYname]] ); print("\n")
    # 1.1.3 print summary
    print( summary( li_ModPoolOLS_NHSS[[tmpYname]] ) )
    print( summary( li_ModPoolGLS_NHSS[[tmpYname]] ) )
    
}





# --------------------------------------------
# SECTION: FIXED INDIVIDUAL EFFECT
# NOTE: in this section, we estimate fixed INDIVIDUAL effect models & random individual effect models for every health outcome
# 
#       using OLS+robustSE for the individual effect models, FGLS estimator for the random effect models;
# 
# NOTE: because we use different individual settings (province for fixed effect mod, county for random effect mod), we cannot perform Haussman test
#       
# 
# ------------------------------

li_ModFix_NHSS <- list()  # for fixed individual effect models
for(tmpYname in envNHSS$Ynames){
    # slice a temp dataset
    tmpdf <- li_Dat_NHSS[[tmpYname]]
    
    # 2.1.1 fixed effect model (FGLS estimator)
    li_ModFix_NHSS[[tmpYname]] <- gls( li_EqFix_NHSS[[tmpYname]], data = tmpdf  )
    
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("FIXED INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(   summary(  li_ModFix_NHSS[[tmpYname]]   )   )
}







# -------------------------------------
## SECTION: RANDOM INDIVIDUAL EFFECT MODEL 
cat("\nSECTION: RANDOM INDIVIDUAL EFFECT MODEL")
# --------------
# 2.0 prepare collections
li_ModRan_NHSS <- list()  # for random individual effect models

# 2.1 loop on health outcomes, estimate & perform Haussman test
for(tmpYname in envNHSS$Ynames){
    # slice a temp dataset
    tmpdf <- li_DatPlm_NHSS[[tmpYname]]
    
    # 2.1.2 random effect model (FGLS estimator)
    li_ModRan_NHSS[[tmpYname]] <- plm::pggls( li_EqRan_NHSS[[tmpYname]], data = tmpdf, 
                                                effect = "individual", model = "random" )
    
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("RANDOM INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(   summary(  li_ModRan_NHSS[[tmpYname]]   )   )
}








# -------------------------------------
## SECTION 3: NOTMALITY OF THE BENCHMARK MODEL (FIXED INDIVIDUAL EFFECT MODEL)
cat("\nSECTION 3: NOTMALITY OF THE BENCHMARK MODEL (FIXED INDIVIDUAL EFFECT MODEL)")
# NOTE: in this section, we test the normality of our benchmark model,
#       the fixed individual effect model.
#       using & output QQplot & common normality tests (depend on library nortest)
# 
#       H0: normal distribution
# 
df_NormTest_NHSS <- list(
    Test = c( "Shapiro-Wilk", "Pearson Chi-square", "Lilliefor",
              "Anderson-Darling", "Cramer-von Mises" )  # test names
)  # saves p-values; converted to a df later

for(tmpYname in envNHSS$Ynames){
    # 3.1 qqplots: plotting & output
    pdf(file = paste(sep="",envNHSS$Output,"ResidQQ_FixIndi_NHSS_",tmpYname,".pdf"), width = 12, height = 10)
        func_easyQQnorm( residuals(li_ModFix_NHSS[[tmpYname]]), ylab = paste(sep="","resid(",tmpYname,")"), alpha = 0.05 )
    dev.off()
    # 3.2 normality tests
    tmp <- residuals(li_ModFix_NHSS[[tmpYname]])
    df_NormTest_NHSS[[tmpYname]] <- c(
        shapiro.test( tmp )$p.value,  # Shapiro-Wilk test
        nortest::pearson.test( tmp )$p.value,  # Peason chi-sqaure test
        nortest::lillie.test( tmp )$p.value,  # Kolmogorov-Smirnov test
        nortest::ad.test( tmp )$p.value,  # Anderson-Darling test
        nortest::cvm.test( tmp )$p.value  # Cramer-von Mises test
    )
}

# 3.3 convert to a dataframe then output
df_NormTest_NHSS <- data.frame(df_NormTest_NHSS)
write.csv(df_NormTest_NHSS,file = paste(sep="",envNHSS$Output,"NormTest_NHSS.csv"))







# -------------------------------------
## SECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL 
cat("\nSECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL ")
# NOTE: in this section, we estimate a two-ways fixed effect model (FGLS),
#       manually construct fixed effect terms then regress the dataset with FGLS estimator
# ----------------------
li_EqTwo_NHSS <- list()
li_ModTwo_NHSS <- list()

# 4.1 estimate & Haussman
for(tmpYname in envNHSS$Ynames){
    # slice a temp data
    tmpdf <- li_Dat_NHSS[[tmpYname]]
    # construct fixed time effect
    tmpdf <- func_MatDesignFixEffect( tmpdf, IndiName = envNHSS$flagT )
    # construct new equation
    tmpxs <- base::setdiff( names(tmpdf), envNHSS$Ynames  )
    li_EqTwo_NHSS[[tmpYname]] <- func_GetEq( tmpYname, tmpxs, KeepStr = FALSE )
    
    # 4.1.1 estimate a two-way fixed effect model
    li_ModTwo_NHSS[[tmpYname]] <- nlme::gls( li_EqTwo_NHSS[[tmpYname]], tmpdf  )
    
    # print the two-way model
    print( summary( li_ModTwo_NHSS[[tmpYname]]  ) )  # summary
    
}




















# -------------------------------------
## SECTION 6: FORMATTED OUTPUT (FOR MS WORD)
# NOTE: in this section, we formatted the results for MS WORD software,
#       then output as excel file (because .csv may lead strings to be recognized as numbers )
# -----------
li_Report_NHSS <- list()  # a list to save formatted reports
for(tmpYname in envNHSS$Ynames){
    # 6.1 temp df
    tmpdf1 <- format_gls( summary(li_ModFix_NHSS[[tmpYname]] , vcov = vcovHC(li_ModFix_NHSS[[tmpYname]])  ) ,
                            Yname = paste(sep="",tmpYname,"_FixIndi_OLS_HCse") )  # fixed  individual effect, OLS estimator + robustSE
    tmpdf2 <- format_pggls( summary(li_ModRan_NHSS[[tmpYname]]) ,
                            Yname = paste(sep="",tmpYname,"_RanIndi_FGLS") )  # random individual effect, FGLS estimator
    tmpdf3 <- format_gls(   summary(li_ModTwo_NHSS[[tmpYname]] ,  vcov = vcovHC(li_ModTwo_NHSS[[tmpYname]]) ) , 
                            Yname = paste(sep="",tmpYname,"_TwoWays_OLS_HCse") )  # two-ways fixed effect model + robustSE
    tmpdf4 <- format_gls(   summary(li_ModPoolGLS_NHSS[[tmpYname]])  ,
                            Yname = paste(sep="",tmpYname,"_Pooling_GLS") )  # Pooling, GLS estimator
    tmpdf5 <- format_lm(    summary(li_ModPoolOLS_NHSS[[tmpYname]])  ,
                            Yname = paste(sep="",tmpYname,"_Pooling_OLS") )  # Pooling, OLS estimator
    # 6.2 merging
    li_Report_NHSS[[tmpYname]] <- plyr::join(
        tmpdf1, tmpdf2,
        by = "Variable", type = "full"
    )
    li_Report_NHSS[[tmpYname]] <- plyr::join(
        li_Report_NHSS[[tmpYname]], tmpdf3,
        by = "Variable", type = "full"
    )
    li_Report_NHSS[[tmpYname]] <- plyr::join(
        li_Report_NHSS[[tmpYname]], tmpdf4,
        by = "Variable", type = "full"
    )
    li_Report_NHSS[[tmpYname]] <- plyr::join(
        li_Report_NHSS[[tmpYname]], tmpdf5,
        by = "Variable", type = "full"
    )
    
}
# 6.3 garbage collection
rm( tmpdf1, tmpdf2, tmpdf3, tmpdf4, tmpdf5 )

# print info
print(li_Report_NHSS)

# output to excel
openxlsx::write.xlsx(li_Report_NHSS, file = paste(sep="",envNHSS$Output,"RegReport_NHSS.xlsx") )









