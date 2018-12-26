

# li_Eq_CHARLS$OUTP1M_RATIO

tmpYname = "OUTP1M_RATIO"   # 9 RC
# tmpYname = "CHRONIC_RATIO"  # 10 RC




# ----------------------------------
tmpeq <- formula( 
    paste(sep="",tmpYname, "~INCOMEASSET_FACTOR + TRANSCHILD_FACTOR + AVGEDU + AVGEDU2 + ",  paste(sep="",collapse = "+", "RC", 1:li_PCAk_CHARLS[[tmpYname]]  )  ) 
    )
# -----------------------------------
# 3.4 construct corresponding datasets
li_Dat_CHARLS <- list()  # general dataset
li_DatPlm_CHARLS <- list()  # plm indexed dataset
for(tmpYname2 in envCHARLS$Ynames){
    # extract principle components of current Y
    tmp <- li_PCAres_CHARLS[[tmpYname2]]$scores
    # merge it with Y, Xcore, tags (indi, time)
    li_Dat_CHARLS[[tmpYname2]] <- data.frame( df_CHARLS[,c( envCHARLS$flagT, envCHARLS$flagCity, tmpYname2, envCHARLS$Xcore, "AVGAGE" )] , tmp )
    li_DatPlm_CHARLS[[tmpYname2]] <- plm::pdata.frame( li_Dat_CHARLS[[tmpYname2]], index = c( envCHARLS$flagCity, envCHARLS$flagT ) )
    
    li_DatPlm_CHARLS[[tmpYname2]]$AVGEDU2 <- ( li_DatPlm_CHARLS[[tmpYname2]]$AVGEDU ) ^ 2
}


# _------------------------------
tmpmod1 <- plm(tmpeq,li_DatPlm_CHARLS[[tmpYname]], effect = "individual", model = "within" )
tmpmod2 <- plm::pggls(tmpeq,li_DatPlm_CHARLS[[tmpYname]], effect = "individual", model = "random" )
tmpmod3 <- plm(tmpeq,li_DatPlm_CHARLS[[tmpYname]], effect = "twoways", model = "within" )
tmpmod4 <- gls( tmpeq, li_DatPlm_CHARLS[[tmpYname]] )
tmpmod5 <- lm( tmpeq, li_DatPlm_CHARLS[[tmpYname]] )
# ---------------------
tmpdf1 <- format_plm( summary(tmpmod1 , vcov = vcovHC(tmpmod1)  ) ,
                      Yname = paste(sep="",tmpYname,"_FixIndi_OLS_HCse") )  # fixed  individual effect, OLS estimator + robustSE
tmpdf2 <- format_pggls( summary(tmpmod2) ,
                        Yname = paste(sep="",tmpYname,"_RanIndi_FGLS") )  # random individual effect, FGLS estimator
tmpdf3 <- format_plm(   summary(tmpmod3 ,  vcov = vcovHC(tmpmod3) ) , 
                        Yname = paste(sep="",tmpYname,"_TwoWays_OLS_HCse") )  # two-ways fixed effect model + robustSE
tmpdf4 <- format_gls(   summary(tmpmod4)  ,
                        Yname = paste(sep="",tmpYname,"_Pooling_GLS") )  # Pooling, GLS estimator
tmpdf5 <- format_lm(    summary(tmpmod5)  ,
                        Yname = paste(sep="",tmpYname,"_Pooling_OLS") )  # Pooling, OLS estimator
# ------------------------------
testdf <- plyr::join(
    tmpdf1, tmpdf2,
    by = "Variable", type = "full"
)
testdf <- plyr::join(
    testdf, tmpdf3,
    by = "Variable", type = "full"
)
testdf <- plyr::join(
    testdf, tmpdf4,
    by = "Variable", type = "full"
)
testdf <- plyr::join(
    testdf, tmpdf5,
    by = "Variable", type = "full"
)
# -------------------------
testdf















