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
# NOTE: if extra normality tests on residuals required, please use the library nortest:: & shapiro.test()

# 1.0 prepare a collection of estimated models
li_ModPoolOLS_NHSS <- list()
li_ModPoolGLS_NHSS <- list()

# 1.1 estimation & print summaries
for(tmpYname in envNHSS$Ynames){
    # 1.1.1 estimation
    li_ModPoolOLS_NHSS[[tmpYname]] <- lm( li_Eq_NHSS[[tmpYname]], dfp_NHSS )
    li_ModPoolGLS_NHSS[[tmpYname]] <- nlme::gls( li_Eq_NHSS[[tmpYname]], dfp_NHSS )
    # 1.1.2 run-time log
    cat("\n---------------------------------------------------------------\n***** FORMULA *****:\n")
    print( li_Eq_NHSS[[tmpYname]] ); print("\n")
    # 1.1.3 print summary
    print( summary( li_ModPoolOLS_NHSS[[tmpYname]] ) )
    print( summary( li_ModPoolGLS_NHSS[[tmpYname]] ) )

}







# -------------------------------------
## SECTION 2: FIXED INDIVIDUAL EFFECT & RANDOM INDIVIDUAL EFFECT MODEL & HAUSSMAN
cat("\nSECTION 2: FIXED INDIVIDUAL EFFECT & RANDOM INDIVIDUAL EFFECT MODEL")
# NOTE: in this section, we estimate fixed INDIVIDUAL effect models & random individual effect models for every health outcome
# 
#       using OLS+robustSE for the individual effect models, FGLS estimator for the random effect models;
# 
#       then, we performed Haussman test, using both original & robust/auxiliary-regression-based test (Wooldridge (2010));
#       the robust Haussman test can cook possible heterosketasticity (well ... we have used FGLS estimators);
# 
# NOTE: based on dfp_NHSS dataset

# 2.0 prepare collections
li_ModFix_NHSS <- list()  # for fixed individual effect models
li_ModRan_NHSS <- list()  # for random individual effect models
df_Haussman_NHSS <- data.frame(
    Equation = envNHSS$Ynames,  # dependents
    Chisq = 0,  # Haussman Statistics (X2)
    pVal_Chisq = 0,  # p-value
    ChisqRobust = 0,  # robust Haussman Statistics (through an auxiliary regression)
    pVal_ChisqRobust = 0  # p-value
)

# 2.1 loop on health outcomes, estimate & perform Haussman test
for(tmpYname in envNHSS$Ynames){
    # 2.1.1 fixed effect model (OLS estimator, the robustSE is computed when summary(mod) )
    li_ModFix_NHSS[[tmpYname]] <- plm::plm( li_Eq_NHSS[[tmpYname]], data = dfp_NHSS, 
                                              effect = "individual", model = "within" )
    # 2.1.2 random effect model (FGLS estimator)
    li_ModRan_NHSS[[tmpYname]] <- plm::pggls( li_Eq_NHSS[[tmpYname]], data = dfp_NHSS, 
                                              effect = "individual", model = "random" )
    
    
    # 2.1.3 Haussman test (H0: random effect, if phtest(fixed, random))
    tmp <- plm::phtest( li_ModFix_NHSS[[tmpYname]], li_ModRan_NHSS[[tmpYname]], method = "chisq" )
    df_Haussman_NHSS$Chisq[df_Haussman_NHSS$Equation == tmpYname] <- tmp$statistic
    df_Haussman_NHSS$pVal_Chisq[df_Haussman_NHSS$Equation == tmpYname] <- tmp$p.value
    # 2.1.4 robust Haussman test (H0: random effect)
    tmp <- plm::phtest( li_ModFix_NHSS[[tmpYname]], li_ModRan_NHSS[[tmpYname]], method = "aux",
                        vcov = function(x) plm::vcovHC(x, method="white2",type="HC3")   )
    df_Haussman_NHSS$ChisqRobust[df_Haussman_NHSS$Equation == tmpYname] <- tmp$statistic
    df_Haussman_NHSS$pVal_ChisqRobust[df_Haussman_NHSS$Equation == tmpYname] <- tmp$p.value
    
    
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("FIXED INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(        summary(  li_ModFix_NHSS[[tmpYname]], vcov = vcovHC( li_ModFix_NHSS[[tmpYname]] )  )          )  # using robust SE
}

# 2.2 print & output the results of Haussman tests
cat("\n---------------------------\n")
cat("HAUSSMAN TEST RESULT; H0: SHOULD USE RANDOM INDIVIDUAL EFFECT","\n")
print(df_Haussman_NHSS)
write.csv(df_Haussman_NHSS, file = paste(sep="",envNHSS$Output,"HaussmanTest_NHSS.csv"))










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
    Test = c( "Shapiro-Wilk", "Pearson Chi-square", "Kolmogorov-Smirnov",
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
## SECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL & CONSISTENCY WITH ONE-WAY FIXED INDIVIDUAL EFFECT MODEL
cat("\nSECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL & CONSISTENCY WITH ONE-WAY FIXED INDIVIDUAL EFFECT MODEL")
# NOTE: in this section, we estimate a two-ways fixed effect model (OLS estimator + robustSE),
#       then perform Haussman test on it & the fixed individual effect model 
#       (H0: the two are consistent, and we have learnt the one-way fixed effect model's estimate is consistent)
#       to see if the estimates of the two models are consistent
li_ModTwo_NHSS <- list()
df_exHaussman_NHSS <- df_Haussman_NHSS  # use the same structure

# 4.1 estimate & Haussman
for(tmpYname in envNHSS$Ynames){
    # 4.1.1 estimate a two-way model
    li_ModTwo_NHSS[[tmpYname]] <- plm::plm( li_Eq_NHSS[[tmpYname]], dfp_NHSS, effect = "twoways", model = "within" )
    # 4.1.2 Haussman test (original)
    tmp <- df_exHaussman_NHSS$Equation == tmpYname
    tmp2 <- plm::phtest( li_ModFix_NHSS[[tmpYname]], li_ModTwo_NHSS[[tmpYname]] )
    df_exHaussman_NHSS$Chisq <- tmp2$statistic
    df_exHaussman_NHSS$pVal_Chisq <- tmp2$p.value
    # 4.1.2 Robust Haussman test (auxiliary regression based)
    tmp2 <- plm::phtest( li_ModFix_NHSS[[tmpYname]], li_ModTwo_NHSS[[tmpYname]], method = "aux" )
    df_exHaussman_NHSS$ChisqRobust <- tmp2$statistic
    df_exHaussman_NHSS$pVal_ChisqRobust <- tmp2$p.value
}

# 4.2 convert the results of Haussman test to a df, then output
df_exHaussman_NHSS <- data.frame(df_exHaussman_NHSS)
write.csv(df_exHaussman_NHSS, file=paste(sep="",envNHSS$Output,"exHaussman_2Wayor1Way_NHSS.csv"))









# -------------------------------------
## SECTION 5: (ROBUST) ADD AN EXTRA CANDIDATE VARIABLE ()
# NOTE: in this section, we add another variable into the final specifications to do robustness check;
#       1st, we check whether the new variable is significant on the three health outcomes?
#       2nd, we add it to the final specifications and regress (benchmark effect, FGLS estimator)














# -------------------------------------
## SECTION 6: FORMATTED OUTPUT (FOR MS WORD)
# NOTE: in this section, we formatted the results for MS WORD software,
#       then output as excel file (because .csv may lead strings to be recognized as numbers )
# -----------
li_Report_NHSS <- list()  # a list to save formatted reports
for(tmpYname in envNHSS$Ynames){
    # 6.1 temp df
    tmpdf1 <- format_plm( summary(li_ModFix_NHSS[[tmpYname]] , vcov = vcovHC(li_ModFix_NHSS[[tmpYname]])  ) ,
                            Yname = paste(sep="",tmpYname,"_FixIndi_OLS_HCse") )  # fixed  individual effect, OLS estimator + robustSE
    tmpdf2 <- format_pggls( summary(li_ModRan_NHSS[[tmpYname]]) ,
                            Yname = paste(sep="",tmpYname,"_RanIndi_FGLS") )  # random individual effect, FGLS estimator
    tmpdf3 <- format_plm(   summary(li_ModTwo_NHSS[[tmpYname]] ,  vcov = vcovHC(li_ModTwo_NHSS[[tmpYname]]) ) , 
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









