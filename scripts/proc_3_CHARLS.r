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
# NOTE: if extra normality tests on residuals required, please use the library nortest:: & shapiro.test()

# 1.0 prepare a collection of estimated models
li_ModPoolOLS_CHARLS <- list()
li_ModPoolGLS_CHARLS <- list()

# 1.1 estimation & print summaries
for(tmpYname in envCHARLS$Ynames){
    # slice a temp dataset
    tmpdf <- li_Dat_CHARLS[[tmpYname]]
    # 1.1.1 estimation
    li_ModPoolOLS_CHARLS[[tmpYname]] <- lm( li_Eq_CHARLS[[tmpYname]], tmpdf )
    li_ModPoolGLS_CHARLS[[tmpYname]] <- nlme::gls( li_Eq_CHARLS[[tmpYname]], tmpdf )
    # 1.1.2 run-time log
    cat("\n---------------------------------------------------------------\n***** FORMULA *****:\n")
    print( li_Eq_CHARLS[[tmpYname]] ); print("\n")
    # 1.1.3 print summary
    print( summary( li_ModPoolOLS_CHARLS[[tmpYname]] ) )
    print( summary( li_ModPoolGLS_CHARLS[[tmpYname]] ) )
    
}









# -------------------------------------
## SECTION 2: FIXED INDIVIDUAL EFFECT & RANDOM INDIVIDUAL EFFECT MODEL & HAUSSMAN
cat("\nSECTION 2: FIXED INDIVIDUAL EFFECT & RANDOM INDIVIDUAL EFFECT MODEL")
# --------------
# 2.0 prepare collections
li_ModFix_CHARLS <- list()  # for fixed individual effect models
li_ModRan_CHARLS <- list()  # for random individual effect models
df_Haussman_CHARLS <- data.frame(
    Equation = envCHARLS$Ynames,  # dependents
    Chisq = 0,  # Haussman Statistics (X2)
    pVal_Chisq = 0,  # p-value
    ChisqRobust = 0,  # robust Haussman Statistics (through an auxiliary regression)
    pVal_ChisqRobust = 0  # p-value
)

# 2.1 loop on health outcomes, estimate & perform Haussman test
for(tmpYname in envCHARLS$Ynames){
    # slice a temp dataset
    tmpdf <- li_DatPlm_CHARLS[[tmpYname]]

    # 2.1.1 fixed effect model (OLS estimator + robustSE)
    li_ModFix_CHARLS[[tmpYname]] <- plm::plm( li_Eq_CHARLS[[tmpYname]], data = tmpdf, 
                                              effect = "individual", model = "within" )
    # 2.1.2 random effect model (FGLS estimator)
    li_ModRan_CHARLS[[tmpYname]] <- plm::pggls( li_Eq_CHARLS[[tmpYname]], data = tmpdf, 
                                              effect = "individual", model = "random" )
    
    # 2.1.3 Haussman test (H0: random effect, if phtest(fixed, random))
    tmp <- plm::phtest( li_ModFix_CHARLS[[tmpYname]], li_ModRan_CHARLS[[tmpYname]], method = "chisq" )
    df_Haussman_CHARLS$Chisq[df_Haussman_CHARLS$Equation == tmpYname] <- tmp$statistic
    df_Haussman_CHARLS$pVal_Chisq[df_Haussman_CHARLS$Equation == tmpYname] <- tmp$p.value
    # 2.1.4 robust Haussman test (H0: random effect)
    tmp <- plm::phtest( li_ModFix_CHARLS[[tmpYname]], li_ModRan_CHARLS[[tmpYname]], method = "aux",
                        vcov = function(x) plm::vcovHC(x, method="white2",type="HC3")   )
    df_Haussman_CHARLS$ChisqRobust[df_Haussman_CHARLS$Equation == tmpYname] <- tmp$statistic
    df_Haussman_CHARLS$pVal_ChisqRobust[df_Haussman_CHARLS$Equation == tmpYname] <- tmp$p.value
    
    # 2.1.4 summary, print the effect models
    cat("\n---------------------------\n")
    cat("FIXED INDIVIDUAL EFFECT MODEL: ",tmpYname,"\n")
    print(   summary(  li_ModFix_CHARLS[[tmpYname]], vcov = vcovHC(li_ModFix_CHARLS[[tmpYname]])  )   )
}

# 2.2 print & output the results of Haussman tests
cat("\n---------------------------\n")
cat("HAUSSMAN TEST RESULT; H0: SHOULD USE RANDOM INDIVIDUAL EFFECT","\n")
print(df_Haussman_CHARLS)
write.csv(df_Haussman_CHARLS, file = paste(sep="",envCHARLS$Output,"HaussmanTest_CHARLS.csv"))








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
## SECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL & CONSISTENCY WITH ONE-WAY FIXED INDIVIDUAL EFFECT MODEL
cat("\nSECTION 4: (ROBUST) TWO-WAYS FIXED EFFECT MODEL & CONSISTENCY WITH ONE-WAY FIXED INDIVIDUAL EFFECT MODEL")
# --------------
li_ModTwo_CHARLS <- list()
df_exHaussman_CHARLS <- df_Haussman_CHARLS  # use the same structure

# 4.1 estimate & Haussman
for(tmpYname in envCHARLS$Ynames){
    # slice a temp data
    tmpdf <- li_DatPlm_CHARLS[[tmpYname]]
    
    # 4.1.1 estimate a two-way model
    li_ModTwo_CHARLS[[tmpYname]] <- plm::plm( li_Eq_CHARLS[[tmpYname]], tmpdf, effect = "twoways", model = "within" )
    # 4.1.2 Haussman test (original)
    tmp <- df_exHaussman_CHARLS$Equation == tmpYname
    tmp2 <- plm::phtest( li_ModFix_CHARLS[[tmpYname]], li_ModTwo_CHARLS[[tmpYname]] )
    df_exHaussman_CHARLS$Chisq <- tmp2$statistic
    df_exHaussman_CHARLS$pVal_Chisq <- tmp2$p.value
    # 4.1.2 Robust Haussman test (auxiliary regression based)
    tmp2 <- plm::phtest( li_ModFix_CHARLS[[tmpYname]], li_ModTwo_CHARLS[[tmpYname]], method = "aux" )
    df_exHaussman_CHARLS$ChisqRobust <- tmp2$statistic
    df_exHaussman_CHARLS$pVal_ChisqRobust <- tmp2$p.value
    
    # print the two-way model
    print( summary( li_ModTwo_CHARLS[[tmpYname]], vcov = vcovHC(li_ModTwo_CHARLS[[tmpYname]])   ) )  # summary, using robust cov-mat estimates
    
}

# 4.2 convert the results of Haussman test to a df, then output
df_exHaussman_CHARLS <- data.frame(df_exHaussman_CHARLS)
write.csv(df_exHaussman_CHARLS, file=paste(sep="",envCHARLS$Output,"exHaussman_2Wayor1Way_CHARLS.csv"))







# -------------------------------------
## SECTION 5: (ROBUST) ADD EXTRA POSSIBLE VARIABLE: 
# NOTE: 
# 
# 
# 
# ----------





# -------------------------------------
## SECTION 6: FORMATTED OUTPUT (FOR MS WORD)
# NOTE: in this section, we formatted the results for MS WORD software,
#       then output as excel file (because .csv may lead strings to be recognized as numbers )
# -----------
li_Report_CHARLS <- list()  # a list to save formatted reports
for(tmpYname in envCHARLS$Ynames){
    # 6.1 temp df
    tmpdf1 <- format_plm( summary(li_ModFix_CHARLS[[tmpYname]] , vcov = vcovHC(li_ModFix_CHARLS[[tmpYname]])  ) ,
                          Yname = paste(sep="",tmpYname,"_FixIndi_OLS_HCse") )  # fixed  individual effect, OLS estimator + robustSE
    tmpdf2 <- format_pggls( summary(li_ModRan_CHARLS[[tmpYname]]) ,
                            Yname = paste(sep="",tmpYname,"_RanIndi_FGLS") )  # random individual effect, FGLS estimator
    tmpdf3 <- format_plm(   summary(li_ModTwo_CHARLS[[tmpYname]] ,  vcov = vcovHC(li_ModTwo_CHARLS[[tmpYname]]) ) , 
                            Yname = paste(sep="",tmpYname,"_TwoWays_OLS_HCse") )  # two-ways fixed effect model + robustSE
    tmpdf4 <- format_gls(   summary(li_ModPoolGLS_CHARLS[[tmpYname]])  ,
                            Yname = paste(sep="",tmpYname,"_Pooling_GLS") )  # Pooling, GLS estimator
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






