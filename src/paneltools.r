# <-- encoding UTF-8 -->
# panel data model tools
# -----------------------------------------
# DOC STRING
# 
# 
# Tianhao Zhao
# Dec 2018
# -----------------------------------------
library(plm)  # popular panel data model library
require(purrr)  # function operation tools, std lib



# -----------------------------------------
# function: check p-values, returns significant marks (e.g. ***)
format_chkp <- function(pvalvec){
    tmpstrvec <- vector(mode="character",length=length(pvalvec))
    for(idx in 1:length(pvalvec)){
        if( pvalvec[idx] < 0.01 ){
            tmpstrvec[idx] <- "***"
        }else if( pvalvec[idx] < 0.05 ){
            tmpstrvec[idx] <- "**"
        }else if( pvalvec[idx] < 0.1 ){
            tmpstrvec[idx] <- "*"
        }else{
            tmpstrvec[idx] <- ""
        }    
    }
    return(tmpstrvec)
}





# -----------------------------------------
# function: formatted a plm model to a data.frame of strings
# NOTE: because we use MS WORD to write papers,
#       it is essential to have this method to avoid manually transcripting regressions results to
#       formatted texts used in MS WORD;
#       if several models there, you may easily combine them in MS EXCEL
format_pggls <- function( summod, digits = 4, Yname = summod$formula[[2]] ){
    # input:
    # 1. summod: a summarized plm model, estimated by plm::pggls (FGLS estimator)
    # output:
    # 1. dfstr: a formatted data.frame, values are transformatted to string (with significance marks, SE in parentheses etc)
    # ---------
    # 1. get sumamrized coefficient table
    coeftab <- summod$CoefTable
    kvar <- nrow(coeftab)  # number of variables
    # 2. construct col 1 (variable names)
    # NOTE: like: [var1; var1 ;var2; var2 ;var3; var3], because we put SEs under coefs+signifmarks, every variable requires two rows
    dfstr <- list(
        Variable = rep(rownames(coeftab), each=2)  # 1st col: variable names
    )
    # NOTE: decorate the rows of SE to distinguish from coef rows, to avoid duplicated rows in merging
    dfstr$Variable[ (1:kvar)*2 ] <- paste(sep="", dfstr$Variable[ (1:kvar)*2 ], "_SE" )
    # 3. use Y name as the title of the 2nd column
    # yname <- summod$formula[[2]] 
    yname <- Yname
    dfstr[[ yname ]] <- 0
    # 4. insert coefficients + significant marks
    # NOTE: convert p-values to string marks (***,**,*)
    dfstr[[yname]][ (1:kvar)*2-1 ] <- paste(sep="", format(round(coeftab[,"Estimate"],digits = digits)), format_chkp( coeftab[,"Pr(>|z|)"] )  )
    # 5. insert SE in parentheses
    # NOTE: round to operate data, format to make the operated data print in uniform digits (the same as the digits in round())
    dfstr[[yname]][ (1:kvar)*2 ] <- paste(sep="","(",format(round(coeftab[,"Std. Error"],digits = digits)),")" )
    
    # 6. convert the list to a dataframe
    dfstr <- data.frame(dfstr)
    # 7. add Multiple R-squared to the last row
    lastrow <- list( Variable = "Multiple R-squared" )
    lastrow[[yname]] <- format(round(summod$rsqr,digits=digits))
    lastrow <- data.frame(lastrow)
    dfstr <- rbind( dfstr, lastrow )
    
    return(dfstr)
}
# ---------------------------------------------
# NOTE: used on the summarized models estimated by plm(), and
# NOTE: two-ways effect models can only by estimated by plm()
format_plm <- function( summod, digits = 4, Yname = formula(summod)[[2]] ){
    # input:
    # 1. summod: a summarized plm model, estimated by plm::pggls (FGLS estimator)
    # output:
    # 1. dfstr: a formatted data.frame, values are transformatted to string (with significance marks, SE in parentheses etc)
    # ---------
    # 1. get sumamrized coefficient table
    coeftab <- summod$coefficients
    kvar <- nrow(coeftab)  # number of variables
    # 2. construct col 1 (variable names)
    # NOTE: like: [var1; var1 ;var2; var2 ;var3; var3], because we put SEs under coefs+signifmarks, every variable requires two rows
    dfstr <- list(
        Variable = rep(rownames(coeftab), each=2)  # 1st col: variable names
    )
    # NOTE: decorate the rows of SE to distinguish from coef rows, to avoid duplicated rows in merging
    dfstr$Variable[ (1:kvar)*2 ] <- paste(sep="", dfstr$Variable[ (1:kvar)*2 ], "_SE" )
    # 3. use Y name as the title of the 2nd column
    # yname <- summod$formula[[2]] 
    yname <- Yname
    dfstr[[ yname ]] <- 0
    # 4. insert coefficients + significant marks
    # NOTE: convert p-values to string marks (***,**,*)
    dfstr[[yname]][ (1:kvar)*2-1 ] <- paste(sep="", format(round(coeftab[,"Estimate"],digits = digits)), format_chkp( coeftab[,"Pr(>|t|)"] )  )
    # 5. insert SE in parentheses
    # NOTE: round to operate data, format to make the operated data print in uniform digits (the same as the digits in round())
    dfstr[[yname]][ (1:kvar)*2 ] <- paste(sep="","(",format(round(coeftab[,"Std. Error"],digits = digits)),")" )
    
    # 6. convert the list to a dataframe
    dfstr <- data.frame(dfstr)
    # 7. add Multiple R-squared to the last 2 rows
    lastrow <- list( Variable = c("R-squared", "Adjusted R-squared" )  )
    lastrow[[yname]] <- c(  format(round(summod$r.squared["rsq"],digits=digits)),
                            format(round(summod$r.squared["adjrsq"],digits=digits))
                        )
    lastrow <- data.frame(lastrow)
    dfstr <- rbind( dfstr, lastrow )
    
    return(dfstr)
}
# ---------------------------------------------
# NOTE: used on the summarized models estimated by nlme::gls()
format_gls <- function( summod, digits = 4, Yname = formula(summod)[[2]] ){
    # input:
    # 1. summod: a summarized plm model, estimated by plm::pggls (FGLS estimator)
    # output:
    # 1. dfstr: a formatted data.frame, values are transformatted to string (with significance marks, SE in parentheses etc)
    # ---------
    # 1. get sumamrized coefficient table
    coeftab <- summod$tTable
    kvar <- nrow(coeftab)  # number of variables
    # 2. construct col 1 (variable names)
    # NOTE: like: [var1; var1 ;var2; var2 ;var3; var3], because we put SEs under coefs+signifmarks, every variable requires two rows
    dfstr <- list(
        Variable = rep(rownames(coeftab), each=2)  # 1st col: variable names
    )
    # NOTE: decorate the rows of SE to distinguish from coef rows, to avoid duplicated rows in merging
    dfstr$Variable[ (1:kvar)*2 ] <- paste(sep="", dfstr$Variable[ (1:kvar)*2 ], "_SE" )
    # 3. use Y name as the title of the 2nd column
    # yname <- summod$formula[[2]] 
    yname <- Yname
    dfstr[[ yname ]] <- 0
    # 4. insert coefficients + significant marks
    # NOTE: convert p-values to string marks (***,**,*)
    dfstr[[yname]][ (1:kvar)*2-1 ] <- paste(sep="", format(round(coeftab[,"Value"],digits = digits)), format_chkp( coeftab[,"p-value"] )  )
    # 5. insert SE in parentheses
    # NOTE: round to operate data, format to make the operated data print in uniform digits (the same as the digits in round())
    dfstr[[yname]][ (1:kvar)*2 ] <- paste(sep="","(",format(round(coeftab[,"Std.Error"],digits = digits)),")" )
    
    # 6. convert the list to a dataframe
    dfstr <- data.frame(dfstr)
    # 7. add Multiple R-squared to the last 2 rows
    lastrow <- list( Variable = c("AIC", "BIC" )  )
    lastrow[[yname]] <- c(  format(round(summod$AIC,digits=digits)),
                            format(round(summod$BIC,digits=digits))
    )
    lastrow <- data.frame(lastrow)
    dfstr <- rbind( dfstr, lastrow )
    
    return(dfstr)
}
# ---------------------------------------------
# NOTE: used on the summarized models estimated by lm(), OLS estimator
format_lm <- function( summod, digits = 4, Yname = formula(summod)[[2]] ){
    # input:
    # 1. summod: a summarized plm model, estimated by plm::pggls (FGLS estimator)
    # output:
    # 1. dfstr: a formatted data.frame, values are transformatted to string (with significance marks, SE in parentheses etc)
    # ---------
    # 1. get sumamrized coefficient table
    coeftab <- summod$coefficients
    kvar <- nrow(coeftab)  # number of variables
    # 2. construct col 1 (variable names)
    # NOTE: like: [var1; var1 ;var2; var2 ;var3; var3], because we put SEs under coefs+signifmarks, every variable requires two rows
    dfstr <- list(
        Variable = rep(rownames(coeftab), each=2)  # 1st col: variable names
    )
    # NOTE: decorate the rows of SE to distinguish from coef rows, to avoid duplicated rows in merging
    dfstr$Variable[ (1:kvar)*2 ] <- paste(sep="", dfstr$Variable[ (1:kvar)*2 ], "_SE" )
    # 3. use Y name as the title of the 2nd column
    # yname <- summod$formula[[2]] 
    yname <- Yname
    dfstr[[ yname ]] <- 0
    # 4. insert coefficients + significant marks
    # NOTE: convert p-values to string marks (***,**,*)
    dfstr[[yname]][ (1:kvar)*2-1 ] <- paste(sep="", format(round(coeftab[,"Estimate"],digits = digits)), format_chkp( coeftab[,"Pr(>|t|)"] )  )
    # 5. insert SE in parentheses
    # NOTE: round to operate data, format to make the operated data print in uniform digits (the same as the digits in round())
    dfstr[[yname]][ (1:kvar)*2 ] <- paste(sep="","(",format(round(coeftab[,"Std. Error"],digits = digits)),")" )
    
    # 6. convert the list to a dataframe
    dfstr <- data.frame(dfstr)
    # 7. add Multiple R-squared to the last 2 rows
    lastrow <- list( Variable = c("R-squared", "Adjusted R-squared" )  )
    lastrow[[yname]] <- c(  format(round(summod$r.squared,digits=digits)),
                            format(round(summod$adj.r.squared,digits=digits))
    )
    lastrow <- data.frame(lastrow)
    dfstr <- rbind( dfstr, lastrow )
    
    return(dfstr)
}











# 
# tmp1 <- summary(li_ModFix_NHSS$illnessratio)
# tmp2 <- summary(li_ModRan_NHSS$illnessratio)
# tmp3 <- summary(li_ModTwo_NHSS$illnessratio)
# tmp4 <- summary(li_ModPoolOLS_NHSS$illnessratio)
# tmp5 <- summary(li_ModPoolGLS_NHSS$illnessratio)
# 
# tmp1$CoefTable
# tmp2$CoefTable
# tmp3$coefficients
# tmp4$coefficients
# tmp5$tTable
# 
# 
# summod <- tmp4






