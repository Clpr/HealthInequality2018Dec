

# construct dataset
df_CHARLS <- df_CHARLS_backup
# df_CHARLS <- func_MatDesignFixEffect( df_CHARLS, IndiName = "province" )
# --------------------------------------


# namelist
tmpyname <- "CHRONIC_RATIO"
tmpinditag <- "province"
tmpxcores <- c("AVGINDIINCOME_EARN","AVGEDU")
tmpxnames <- c(   # prepare to select & PCA
    "AVGAGE", "MALE_RATIO", "MARITAL_RATIO", "MARITAL_AVELEN",  "URBAN_RATIO",  
    "AVGBMI",  "DRINK1Y_RATIO", "SMOKEEVER_RATIO",  
    "SMOKENOW_RATIO",    "AVGSMOKENUM",   "AVGHOSP1Y_REALEXP", "AVGOUTP1M_REALEXP", "INSURANCE_RATIO",   "INSGOV_RATIO",  "INSPRI_RATIO",  "AVGEXP1W_FOOD",    
    "AVGEXP1Y_TOTAL",    "CHILDCARE_RATIO",   "CHILDCORESD_RATIO", "CHILDLVNEAR_RATIO", "SOCWK_RATIO",   "TRANSCHILD_RATIO",  "WORK_RATIO",    
    "JOBSTATUS_AGRI_RATIO", "JOBSTATUS_NAGE_RATIO", "JOBSTATUS_NAGS_RATIO", "JOBSTATUS_NAGF_RATIO", "JOBSTATUS_UNEM_RATIO", "JOBSTATUS_NEWK_RATIO"
)
# --------------------------------------


# single variable selection
tmpxnames2 <- c()
for(tmpx in tmpxnames){
    tmpeq <- func_GetEq( tmpyname, c( tmpxcores, tmpx ), KeepStr = FALSE )
    tmpmod <- gls(tmpeq,df_CHARLS)
    if(  summary(tmpmod)$tTable[tmpx,4] < 0.05 ){  # if significant at alpha = 0.05
        tmpxnames2 <- append(tmpxnames2, tmpx)  # add to specifcation
    }
}
# -------------------------------------


library(psych)  # easy PCA
# PCA to summarize control variables
tmpdf <- df_CHARLS[,tmpxnames2]
tmpres <- psych::principal( tmpdf, nfactors = 7, 
                            rotate = "varimax", scores = TRUE )  # PCA analysis
print(tmpres)  # summary
plot( tmpres$values ); lines(1:length(tmpres$values), rep.int(1,length(tmpres$values)), col = "red" )
# -------------------------------------

# construct regression dataset
df_CHARLS <- cbind( df_CHARLS[,c(tmpyname,tmpxcores,tmpinditag)],  tmpres$scores   )  # y + components + individual tags

df_CHARLS$AVGINDIINCOME_EARN2 <- ( df_CHARLS$AVGINDIINCOME_EARN )^2

# parse individual tags to a designed dataframe
df_CHARLS <- func_MatDesignFixEffect( df_CHARLS, IndiName = tmpinditag )
# get the names of regressors
tmpxnames <- base::setdiff( names(df_CHARLS), tmpyname )

tmpxnames <- c( tmpxnames, "AVGINDIINCOME_EARN2" )

# construct regression equation
regeq <- func_GetEq( tmpyname, tmpxnames, KeepStr = FALSE )
# --------------------------------------




# regression (GLS)
tmpmod <- nlme::gls( regeq, df_CHARLS )
summary(tmpmod)
# ---------------------------------------



source("./src/paneltools.r")
tmp <- format_gls(summary(tmpmod),
                  Yname = paste(sep="",tmpyname,"_FixIndi_FGLS")  )


tmp











