# <-- encoding UTF-8 -->
# Empirical study on CHARLS dataset (city level)
# -------------------------------------
## DOC STRING
# 
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------

# -------------------------------------
## SECTION 0: ENVIRONMENT
library(sqldf)  # sql enquiry
library(openxlsx)  # easy xlsx IO
source("./src/mathtools.r")  # math tools, e.g. Lorenz, Theil, descriptive stats
source("./src/mapplots.r")  # tools of map plots, using ggplot2
# environment par dict
envCHARLS$Output <- "./output/proc_2_CHARLS/"  # alter output directory



# -------------------------------------
## SECTION 1: GENERAL DESCRIPTIVE STATISTICS
cat("\nGeneral descriptive statistics of CHARLS dataset:\n")
cat("(please note: the income now is the residuals of income ~ edu)\n")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
cat("PLEASE NOTE: because we have normalized PCA components, we only do descriptive statistics for income & edu!!!!!!")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
# --------------
li_Descript_CHARLS <- list()  # a list of descriptive statistics
for(tmpYname in envCHARLS$Ynames){
    # 1.1 get a namelist of all numeric variables, both Y and Xcore
    tmp <- c( tmpYname, envCHARLS$Xcore )
    li_Descript_CHARLS[[tmpYname]] <- func_DescrStat( li_Dat_CHARLS[[tmpYname]][,tmp] )
    # 1.2 print
    print(li_Descript_CHARLS[[tmpYname]]); cat("-----------------------------------------\n")
}
# output to xlsx
openxlsx::write.xlsx(li_Descript_CHARLS, paste(sep="",envCHARLS$Output,"Descript_CHARLS.xlsx")  )





# -------------------------------------
## SECTION 2: city-LEVEL LORENZ CURVE & GINI COEF
# ---------
# 2.0 prepare a df for Gini coefs
df_Gini_CHARLS <- data.frame(
    year = c(2011, 2013, 2015),   # years
    cities = c( sum(df_CHARLS$year == 2011),sum(df_CHARLS$year == 2013),sum(df_CHARLS$year == 2015)  ),  # number of cities in every year's data
    OUTP1M_RATIO = NA,
    CHRONIC_RATIO = NA
)

# 2.1 use functions in mathtools.r to compute Lorenz Curve & Gini coefficients
# NOTE: please refer to mathtools.r for how we calculated the both
#       and, if you want to see the plotting of Lorenz Curve, you may DIY (using tmp$LorenzX and tmp$LorenzY)
for(tmpYear in df_Gini_CHARLS$year){
    for(tmpYname in envCHARLS$Ynames){
        # 2.1.1 compute Lorenz Curve & Gini in specific year
        # eval(parse(text=paste(sep="",
        #     "tmp <- LorenzCurve( df_CHARLS$",tmpYname,"[df_CHARLS$",envCHARLS$flagT," == ",tmpYear,"] )"
        # ))) 
        tmp <- li_Dat_CHARLS[[tmpYname]]
        tmp <- LorenzCurve( tmp[ tmp[,envCHARLS$flagT] == tmpYear ,tmpYname] )
        # 2.1.2 save Gini coef
        df_Gini_CHARLS[,tmpYname][df_Gini_CHARLS$year == tmpYear] <- tmp$GiniIdx
    }
}
# print & output to xlsx
print(df_Gini_CHARLS)

# NOTE: the results will be output to a csv file with other kinds of indices, later







# -------------------------------------
## SECTION 3: CITY-LEVEL THEIL I & II INDEXES
# --------
# 3.0 prepare two df
df_Theil1_CHARLS <- data.frame(
    year = c(2011, 2013, 2015),   # years
    cities = c( sum(df_CHARLS$year == 2011),sum(df_CHARLS$year == 2013),sum(df_CHARLS$year == 2015)  ),  # number of counties in every year's data
    OUTP1M_RATIO = NA, 
    CHRONIC_RATIO = NA
)
df_Theil2_CHARLS <- df_Theil1_CHARLS

# 3.1 use functions in mathtools.r to do computing
for(tmpYear in df_Gini_CHARLS$year){  # loop on years
    for(tmpYname in envCHARLS$Ynames){  # loop on health outcomes
        # slice a temp dataset
        tmpdf <- li_Dat_CHARLS[[tmpYname]]
        # 3.1.1 compute Theil-I & Theil-II in a specific year
        tmp <- Theil( tmpdf[,tmpYname][tmpdf[,envCHARLS$flagT] == tmpYear], Type = "T" )
        df_Theil1_CHARLS[,tmpYname][ df_Theil1_CHARLS$year == tmpYear ] <- tmp
        tmp <- Theil( tmpdf[,tmpYname][tmpdf[,envCHARLS$flagT] == tmpYear], Type = "L" )
        df_Theil2_CHARLS[,tmpYname][ df_Theil2_CHARLS$year == tmpYear ] <- tmp
    }
}

# 3.2 print info
cat("\nThe city-level Theil-I type index in every year of CHARLS dataset are:\n")
print(df_Theil1_CHARLS)
cat("\nThe city-level Theil-II type index in every year of CHARLS dataset are:\n")
print(df_Theil2_CHARLS)

# NOTE: the results will be output to a csv file with other kinds of indices, later







# -------------------------------------
## SECTION 4: CITY-LEVEL COEF OF VARIANCE (C.V.) & VARIANCE
# ----------
# 4.0 prepare two df
df_CoefVar_CHARLS <- data.frame(
    year = c(2011, 2013, 2015),   # years
    cities = c( sum(df_CHARLS$year == 2011),sum(df_CHARLS$year == 2013),sum(df_CHARLS$year == 2015)  ),  # number of counties in every year's data
    OUTP1M_RATIO = NA,
    CHRONIC_RATIO = NA
)
df_Variance_CHARLS <- df_CoefVar_CHARLS

# 4.1 calculation
for(tmpYear in df_Gini_CHARLS$year){  # loop on years
    for(tmpYname in envCHARLS$Ynames){  # loop on health outcomes
        # slice a temp dataset
        tmpdf <- li_Dat_CHARLS[[tmpYname]]
        # 4.1.1 get data vector
        tmp <- tmpdf[,tmpYname][ tmpdf[,envCHARLS$flagT] == tmpYear ]
        df_Variance_CHARLS[,tmpYname][df_Variance_CHARLS$year == tmpYear] <- var(tmp)
        df_CoefVar_CHARLS[,tmpYname][df_CoefVar_CHARLS$year == tmpYear] <- sd(tmp) / mean(tmp)
    }
}
# 4.2 print info
cat("\nThe county-level coef of variance in every year of CHARLS dataset are:\n")
print(df_CoefVar_CHARLS)
cat("\nThe county-level variances in every year of CHARLS dataset are:\n")
print(df_Variance_CHARLS)






# -------------------------------------
## SECTION 5: COLLECT INEQUALITY INDICES & OUTPUT TO CSV
# ---------------
# 5.1 table joining
df_InequalIdx_CHARLS <- data.frame( Index = rep(c("Gini","Theil-I","Theil-II","Variance","Coef of Variance"), each = 3) )
df_InequalIdx_CHARLS <- cbind(
    df_InequalIdx_CHARLS, rbind( df_Gini_CHARLS, df_Theil1_CHARLS, df_Theil2_CHARLS, df_Variance_CHARLS, df_CoefVar_CHARLS )
)
# 5.2 garbage recycling
rm( df_Gini_CHARLS, df_Theil1_CHARLS, df_Theil2_CHARLS, df_Variance_CHARLS, df_CoefVar_CHARLS )
rm( tmpYear, tmpYname )
# 5.3 output to csv
write.csv( df_InequalIdx_CHARLS, file = paste(sep="",envCHARLS$Output,"InequalityIdx_CHARLS.csv") )








# -------------------------------------
## INTERVAL: MATCH CITY GIS DATA WITH CHARLS DATASET
# -------------
tmpdf <- sqldf::sqldf("SELECT DISTINCT MapGIScity.long as long, MapGIScity.lat as lat, df_CHARLS.* FROM df_CHARLS,MapGIScity WHERE df_CHARLS.city = MapGIScity.county ")
# tmpdf <- plyr::join( df_CHARLS, MapGIScity[,c("ID","long","lat")], by = "city" )  # we use GB (country standard) codes of city/county to match by
tmpdf <- na.omit(tmpdf)


cat("\nPlotting ...\n")
# -------------------------------------
## SECTION 6: COLORED MAPS OF HEALTH OUTCOMES
# ------------
# 6.1 colored maps
# 2011,2013,2015 data are now seen as a pool dataset
for(tmpYname in envCHARLS$Ynames){
    # 6.1.1 temp slice: pooled health outcome & province tags
    tmp <- tmpdf[,c(tmpYname,envCHARLS$flagProv, "city", "long", "lat" )]

    # income -> health outcome
    tmpfig1 <- func_MapCityXY( tmpdf$AVGINDIINCOME_EARN, tmpdf[,tmpYname], tmpdf$lat, tmpdf$long, 
                               Xname = "AVGINDIINCOME_EARN", Yname = tmpYname , FontSize = 5,
                               ColorScale = c("skyblue","red"), CircleScale = c(2,15) )
    # edu -> health outcome
    tmpfig2 <- func_MapCityXY( tmpdf$AVGEDU, tmpdf[,tmpYname], tmpdf$lat, tmpdf$long, 
                               Xname = "AvgEdu", Yname = tmpYname , FontSize = 5,
                               ColorScale = c("skyblue","red"), CircleScale = c(2,15) )
    # output figures
    eval(parse(text=paste(sep="",
                          "func_SaveMap2PDF( tmpfig1, \"",envCHARLS$Output,"Map_AvgIndiIncome_2_",tmpYname,".pdf\" ) "
    )))
    eval(parse(text=paste(sep="",
                          "func_SaveMap2PDF( tmpfig2, \"",envCHARLS$Output,"Map_AvgEdu_2_",tmpYname,".pdf\" ) "
    )))
}

# 6.2 print info
cat("\n We have created colored maps for health outcomes and output them to the assigned output directory as PDF figures\n")











# --------------------------------------
# GARBAGE COLLECTION
rm( tmp, tmpfig1, tmpfig2, tmpYname, tmpdf )







