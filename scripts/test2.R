tmplist <- c(
    "AVGHOUSINCOME_TOTAL",
    "AVGHOUSINCOME_CAPITAL" ,
    # --------------
    "AVGINDIINCOME_TOTAL",
    "AVGINDIINCOME_EARN" ,
    "AVGINDIINCOME_SELF" ,
    "AVGINDIINCOME_PENSION" ,
    "AVGINDIINCOME_TRANS" ,
    "AVGINDIINCOME_OTHER" ,
    "AVGINDIINCOME_FAMILY"
    # -----------
)
tmplisty <- c( "OUTP1M_RATIO", "CHRONIC_RATIO" )

tmpdat <- df_CHARLS_backup[,c(tmplist,tmplisty)]



pairs( tmpdat, cex.labels = 1.2 )




tmpdat <- pdata.frame( df_CHARLS_backup, index = c( envCHARLS$flagCity, envCHARLS$flagT ) )

tmpmod1 <- plm( CHRONIC_RATIO ~AVGINDIINCOME_EARN, tmpdat, effect = "twoways", model = "within" )
tmpmod2 <- pggls( CHRONIC_RATIO ~AVGINDIINCOME_EARN, tmpdat, effect = "individual", model = "random" )
phtest( tmpmod1, tmpmod2 )

summary(tmpmod1, vcov = vcovHC(tmpmod1) )
summary(tmpmod2)



