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
library(sqldf)  # sql enquiry
library(openxlsx)  # easy xlsx IO
source("./src/mathtools.r")  # math tools, e.g. Lorenz, Theil, descriptive stats
source("./src/mapplots.r")  # tools of map plots, using ggplot2
# environment par dict
envNHSS$Output <- "./output/proc_2_NHSS/"  # alter output directory



# -------------------------------------
## SECTION 1: GENERAL DESCRIPTIVE STATISTICS
cat("\nGeneral descriptive statistics of NHSS dataset:\n")
cat("(please note: the income now is the residuals of income ~ edu)\n")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
cat("PLEASE NOTE: because we have normalized PCA components, we only do descriptive statistics for income & edu!!!!!!")
cat("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
# --------------
li_Descript_NHSS <- list()  # a list of descriptive statistics
for(tmpYname in envNHSS$Ynames){
    # 1.1 get a namelist of all numeric variables, both Y and Xcore
    tmp <- c( tmpYname, envNHSS$Xcore )
    li_Descript_NHSS[[tmpYname]] <- func_DescrStat( li_Dat_NHSS[[tmpYname]][,tmp] )
    # 1.2 print
    print(li_Descript_NHSS[[tmpYname]]); cat("-----------------------------------------\n")
}
# output to xlsx
openxlsx::write.xlsx(li_Descript_NHSS, paste(sep="",envNHSS$Output,"Descript_NHSS.xlsx")  )






# -------------------------------------
## SECTION 2: COUNTY-LEVEL LORENZ CURVE & GINI COEF
# NOTE: in this section, we use counties as units/individuals to plot Lorenz curves in every year (98,03,08),
#       meanwhile, Gini coefficients in the three years are calculated;
#       later, in following sections, we will add other inequlity indices

# 2.0 prepare a df for Gini coefs
df_Gini_NHSS <- data.frame(
    year = c(1998, 2003, 2008),   # years
    counties = c( sum(df_NHSS$year == 1998),sum(df_NHSS$year == 2003),sum(df_NHSS$year == 2008)  ),  # number of counties in every year's data
    illnessratio = NA, # prevalence
    illnessday = NA,
    chronicratio = NA
)

# 2.1 use functions in mathtools.r to compute Lorenz Curve & Gini coefficients
# NOTE: please refer to mathtools.r for how we calculated the both
#       and, if you want to see the plotting of Lorenz Curve, you may DIY (using tmp$LorenzX and tmp$LorenzY)
for(tmpYear in df_Gini_NHSS$year){
    for(tmpYname in envNHSS$Ynames){
        # 2.1.1 compute Lorenz Curve & Gini in specific year
        # eval(parse(text=paste(sep="",
        #     "tmp <- LorenzCurve( df_NHSS$",tmpYname,"[df_NHSS$",envNHSS$flagT," == ",tmpYear,"] )"
        # ))) 
        tmp <- li_Dat_NHSS[[tmpYname]]
        tmp <- LorenzCurve( tmp[ tmp[,envNHSS$flagT] == tmpYear ,tmpYname] )
        # 2.1.2 save Gini coef
        df_Gini_NHSS[,tmpYname][df_Gini_NHSS$year == tmpYear] <- tmp$GiniIdx
    }
}
# print & output to xlsx
print(df_Gini_NHSS)


# NOTE: the results will be output to a csv file with other kinds of indices, later



# -------------------------------------
## SECTION 3: COUNTY-LEVEL THEIL I & II INDEXES
# NOTE: in this section, we use counties as units/individuals to calculate Theil-I & Theil-II indexes.
#       meanwhile, indices in the three years are calculated;
#       later, in following sections, we will add other inequlity indices

# 3.0 prepare two df
df_Theil1_NHSS <- data.frame(
    year = c(1998, 2003, 2008),   # years
    counties = c( sum(df_NHSS$year == 1998),sum(df_NHSS$year == 2003),sum(df_NHSS$year == 2008)  ),  # number of counties in every year's data
    illnessratio = NA, # prevalence
    illnessday = NA,
    chronicratio = NA
)
df_Theil2_NHSS <- df_Theil1_NHSS

# 3.1 use functions in mathtools.r to do computing
for(tmpYear in df_Gini_NHSS$year){  # loop on years
    for(tmpYname in envNHSS$Ynames){  # loop on health outcomes
        # slice a temp dataset
        tmpdf <- li_Dat_NHSS[[tmpYname]]
        # 3.1.1 compute Theil-I & Theil-II in a specific year
        tmp <- Theil( tmpdf[,tmpYname][tmpdf[,envNHSS$flagT] == tmpYear], Type = "T" )
        df_Theil1_NHSS[,tmpYname][ df_Theil1_NHSS$year == tmpYear ] <- tmp
        tmp <- Theil( tmpdf[,tmpYname][tmpdf[,envNHSS$flagT] == tmpYear], Type = "L" )
        df_Theil2_NHSS[,tmpYname][ df_Theil2_NHSS$year == tmpYear ] <- tmp
    }
}

# 3.2 print info
cat("\nThe county-level Theil-I type index in every year of NHSS dataset are:\n")
print(df_Theil1_NHSS)
cat("\nThe county-level Theil-II type index in every year of NHSS dataset are:\n")
print(df_Theil2_NHSS)

# NOTE: the results will be output to a csv file with other kinds of indices, later





# -------------------------------------
## SECTION 4: COUNTY-LEVEL COEF OF VARIANCE (C.V.) & VARIANCE
# NOTE: in this section, we use counties as units/individuals to calculate C.V. & variances
#       meanwhile, indices in the three years are calculated;

# 4.0 prepare two df
df_CoefVar_NHSS <- data.frame(
    year = c(1998, 2003, 2008),   # years
    counties = c( sum(df_NHSS$year == 1998),sum(df_NHSS$year == 2003),sum(df_NHSS$year == 2008)  ),  # number of counties in every year's data
    illnessratio = NA, # prevalence
    illnessday = NA,
    chronicratio = NA
)
df_Variance_NHSS <- df_CoefVar_NHSS

# 4.1 calculation
for(tmpYear in df_Gini_NHSS$year){  # loop on years
    for(tmpYname in envNHSS$Ynames){  # loop on health outcomes
        # slice a temp dataset
        tmpdf <- li_Dat_NHSS[[tmpYname]]
        # 4.1.1 get data vector
        tmp <- tmpdf[,tmpYname][ tmpdf[,envNHSS$flagT] == tmpYear ]
        df_Variance_NHSS[,tmpYname][df_Variance_NHSS$year == tmpYear] <- var(tmp)
        df_CoefVar_NHSS[,tmpYname][df_CoefVar_NHSS$year == tmpYear] <- sd(tmp) / mean(tmp)
    }
}

# 4.2 print info
cat("\nThe county-level coef of variance in every year of NHSS dataset are:\n")
print(df_CoefVar_NHSS)
cat("\nThe county-level variances in every year of NHSS dataset are:\n")
print(df_Variance_NHSS)





# -------------------------------------
## SECTION 5: COLLECT INEQUALITY INDICES & OUTPUT TO CSV
# NOTE: in this section, we firstly bind all kinds of inequality indices to a single dframe,
#       then, output the large dframe to a csv file

# 5.1 table joining
df_InequalIdx_NHSS <- data.frame( Index = rep(c("Gini","Theil-I","Theil-II","Variance","Coef of Variance"), each = 3) )
df_InequalIdx_NHSS <- cbind(
    df_InequalIdx_NHSS, rbind( df_Gini_NHSS, df_Theil1_NHSS, df_Theil2_NHSS, df_Variance_NHSS, df_CoefVar_NHSS )
)
# 5.2 garbage recycling
rm( df_Gini_NHSS, df_Theil1_NHSS, df_Theil2_NHSS, df_Variance_NHSS, df_CoefVar_NHSS )
rm( tmpYear, tmpYname, tmpdf )
# 5.3 output to csv
write.csv( df_InequalIdx_NHSS, file = paste(sep="",envNHSS$Output,"InequalityIdx_NHSS.csv") )
















# -------------------------------------
## INTERVAL: MATCH CITY GIS DATA WITH NHSS DATASET
# NOTE: in this section, we merge the China city/County GIS data with df_NHSS;
#       the data have been preloaded by mapplots.r, and named as MapGIScity;
#       in the next section, the dataset will be used to plot
# NOTE: we do not plot PCs -> health outcomes, therefore, just use df_NHSS!
tmpdf <- plyr::join( df_NHSS, MapGIScity[,c("ID","long","lat")], by = "ID" )  # we use GB (country standard) codes of city/county to match by
tmpdf <- na.omit(tmpdf)


cat("\nPlotting ...\n")
# -------------------------------------
## SECTION 6: COLORED MAPS OF HEALTH OUTCOMES
# NOTE: in this section, we plot colored maps for every health outcomes,
#       where every health outcome (in three years) are seen as a pool data,
#       to see the (geographic) distribution (descriptively, intuitively) of the outcomes;
#       using ggplot2, and tools in mapplots.r
#       finally, we output these maps/figures to PDF figures
# NOTE: please refer to mapplots.r for more information & technical details
# NOTE: in final plots, the size of circle indicates the values of independents, where the color indicates the values of health outcomes
#       the plots work to display the relationship between X & Y
# --------
# 6.1 colored maps
# 1998,2003,2008 data are now seen as a pool dataset
for(tmpYname in envNHSS$Ynames){
    # 6.1.1 temp slice: pooled health outcome & province tags
    tmp <- tmpdf[,c(tmpYname,envNHSS$flagProv, "ID", "long", "lat" )]
        # # 6.1.2 averaging by procinve tags (weighted by sample size/population of each county)
        # tmp <- sqldf::sqldf(paste(sep="",
        #       "SELECT DISTINCT SUM(",envNHSS$flagSampleSize," * ",tmpYname,") / SUM(",envNHSS$flagSampleSize,") AS ",tmpYname,
        #       ", ",envNHSS$flagProv," FROM tmp GROUP BY ",envNHSS$flagProv
        # ))
        # 6.1.3 using mapplots.r to get an instance of current map figure
        # eval(parse(text=paste(sep="",
        #     "tmpfig <- func_MapProv( tmp$",tmpYname,", tmp$",envNHSS$flagProv,", vecName = \"",tmpYname,"\" )"
        # )))
    # income -> health outcome
    tmpfig1 <- func_MapCityXY( tmpdf$income, tmpdf[,tmpYname], tmpdf$lat, tmpdf$long, 
                               Xname = "income", Yname = tmpYname , FontSize = 5,
                               ColorScale = c("skyblue","red"), CircleScale = c(2,15) )
    # edu -> health outcome
    tmpfig2 <- func_MapCityXY( tmpdf$edu, tmpdf[,tmpYname], tmpdf$lat, tmpdf$long, 
                               Xname = "edu", Yname = tmpYname , FontSize = 5,
                               ColorScale = c("skyblue","red"), CircleScale = c(2,15) )
    # output figures
    eval(parse(text=paste(sep="",
       "func_SaveMap2PDF( tmpfig1, \"",envNHSS$Output,"Map_income_2_",tmpYname,".pdf\" ) "
    )))
    eval(parse(text=paste(sep="",
       "func_SaveMap2PDF( tmpfig2, \"",envNHSS$Output,"Map_edu_2_",tmpYname,".pdf\" ) "
    )))
}

# 6.2 print info
cat("\n We have created colored maps for health outcomes and output them to the assigned output directory as PDF figures\n")








# --------------------------------------
# GARBAGE COLLECTION
rm( tmp, tmpfig1, tmpfig2, tmpYname, tmpdf )

























# 
# # -------------------------------------
# ## SECTION 7: COLORED MAPS OF CORE INDEPENDENTS (INCOME & EDU)
# # NOTE: please refer to mapplots.r for more information & technical details
# 
# # 7.1 colored maps
# # 1998,2003,2008 data are now seen as a pool dataset
# for(tmpXname in envNHSS$Xcore){
#     # 7.1.1 temp slice: pooled health outcome & province tags
#     tmp <- df_NHSS[,c(tmpXname,envNHSS$flagProv,envNHSS$flagSampleSize)]
#     # 7.1.2 averaging by procinve tags
#     tmp <- sqldf::sqldf(paste(sep="",
#                               "SELECT DISTINCT SUM(",envNHSS$flagSampleSize," * ",tmpXname,") / SUM(",envNHSS$flagSampleSize,") AS ",tmpXname,
#                               ", ",envNHSS$flagProv," FROM tmp GROUP BY ",envNHSS$flagProv
#     ))
#     # 7.1.3 using mapplots.r to get an instance of current map figure
#     eval(parse(text=paste(sep="",
#                           "tmpfig <- func_MapProv( tmp$",tmpXname,", tmp$",envNHSS$flagProv,", vecName = \"",tmpXname,"\" )"
#     )))
#     # 7.1.4 output figures
#     eval(parse(text=paste(sep="",
#                           "func_SaveMap2PDF( tmpfig, \"",envNHSS$Output,"Map_",tmpXname,".pdf\" ) "
#     )))
# }
# 
# # 7.2 print info
# cat("\nWe have created colored maps for core independents (income & edu) and output them to the assigned output directory as PDF figures\n")



























