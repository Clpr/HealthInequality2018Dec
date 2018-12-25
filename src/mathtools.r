# <-- encoding UTF-8 -->
# mathmematical tools
# ------------------------------------
## DOC STRING
# easy math tools (e.g. Lorenz curve, Gini, Theil-I & II)
# 
# all tools were developed by Tianhao Zhao and require less dependencies (actually, no extra package needed);
# MIT license;
# pls acknowledge him when using these functions;
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# ------------------------------------




# ------------------------------------
## function: given x,y vectors, computes trapezium integral (lower & upper bound decided by x iteself); 
# return a real number INT; not support infinite integral; no dependency
TrapeInt <- function(x,y){
    # get length, validation
    N <- if(length(x)==length(y)) length(x) else stop("x,y should have the same length")
    # sort by x
    chk <- sort(x,index.return=TRUE)$ix
    x <- x[chk]; y <- y[chk]
    # loop and compute
    INT <- 0
    for(idx in 1:(N-1)){
        INT <- INT + 0.5 * ( y[idx+1]+y[idx] ) * ( x[idx+1]-x[idx] )
    }
    return(INT)
}


# ------------------------------------
## function: compute y's Lorenz Curve & Gini value, sorted by x, weighted by w (popualtion, default all 1); y must be non-negative
LorenzCurve <- function(y,x=y,w=1+vector(mode="numeric",length=length(y))){
    # input:
    # 1. y: a numeric vector of income (or equivalent value)
    # 2. x: a numeric vector of factor; if y==x, Lorenz Curve & Gini, otherwise, Concentration Curve & Concentration Rate; by default, x == y
    # 3. w: a numeric vector of weights on y (frequencies of each element of y); by default, all 1 (each element of y is an individual but not a representation of a group)
    # output:
    # 1. list:
    #   1.1 LorenzX: X vector to plot Lorenz Curve (or Concentration Curve); in percentile measure, from 0 to 100
    #   1.2 LorenzY: Y vector to plot Lorenz Curve (or Concentration Curve); in percentile measure, from 0 to 100
    #   1.3 GiniIdx: real number in range [0,1]; the Gini index (or concentration index)
    # dependency:
    # 1. TrapeInt(): defined ad hoc, in this module; does trapezium integral for (x,y) pairs
    # -------------------
    # get length
    N <- if(all(y>=0)) length(y) else stop("y >=0 required")
    # sorting & getting index (increasing)
    chk <- sort(x,index.return=TRUE)$ix
    # new y extended by w times
    y <- rep(y[chk],times=w)
    N <- length(y)
    # Lorenz curve (discrete), x in 1:100 (non-constant space), y=1:100(non-constant space)
    LorenzX <- c(0, (1:N)/N * 100)   # add a 0 to make two lines(Lorenz & equality line) meet at 0)
    LorenzY <- c(0, cumsum(y) / sum(y) * 100 )
    # using trapezium integral to compute approximated Gini/concentration index
    GiniIdx <- 1 - TrapeInt(LorenzX,LorenzY)/5000
    # return
    return(list(LorenzX=LorenzX,LorenzY=LorenzY,GiniIdx=GiniIdx))
}


# ------------------------------------
## function: compute non-negative y's Theil-T (a=1) & Theil-L (a=0) index, 
Theil <- function(y,w=1+vector(mode="numeric",length=length(y)), Type = "T"  ){
    # input:
    # 1. y: non-negative numeric vector, data to compute Theil index (individual-level data or grouped data with frequency w)
    # 2. w: weight/frequency vector
    # 3. Type: type of Theil index, "T" or "L" allowed (I & II)
    # output:
    # 1. a number, Theil index
    # dependency:
    # 1. NULL
    # ----------------
    # validation
    if(any(y<0)) stop("y>=0 required")
    # expand to micro level data (replicate by population w)
    y <- rep(y,times=w)
    N <- length(y)
    # get Theil index
    ybar <- mean(y)
    if(Type == "T"){
        return( mean(y/ybar * log((1E-12) + y/ybar)) )  # a minor is added to avoid y = 0
    }else if(Type == "L"){
        return( mean( log((1E-12) + ybar / (y+(1E-12))  ) ) )
    }else{
        stop("invalid type of Theil index")
    }
    # nomial return
    return(NULL)
}



# ------------------------------------
## function: regular descriptive statistics for a dataframe (all variables are numeric)
func_DescrStat <- function(tmpdf){
    return(data.frame(
        Variable = names(tmpdf),
        Observations = apply(tmpdf, 2, length ),
        Mean = apply(tmpdf, 2, mean ),
        Stdev = apply(tmpdf, 2, sd ),
        Min = apply(tmpdf, 2, min ),
        Q1 = apply(tmpdf, 2, quantile, probs = 0.25 ),
        Median = apply(tmpdf, 2, quantile, probs = 0.50 ),
        Q3 = apply(tmpdf, 2, quantile, probs = 0.75 ),
        Max = apply(tmpdf, 2, max )
    ))
}









# ------------------------------------
# function: construct formula objects from Y's name and a vector of X candidates
# NOTE: not support complex setting, e.g. instrumental variables
func_GetEq <- function(Yname, Xnames, Intercept = TRUE, KeepStr = TRUE ){
    # input:
    # 1. Yname: a string, indicates Y
    # 2. Xnames: a char vec of X names
    # 3. Intercept: bool, indicates if to keep an intercept
    # 4. KeepStr: bool, if TRUE, returns a string formula, if FALSE, returns a formula instance
    # output：
    # 1. Eq: a formla instance
    # ----------
    Eq <- paste(Yname,"~", paste(collapse=" + ",Xnames) )
    if(Intercept){
        Eq <- Eq
    }else{
        Eq <- paste(Eq,"-1")
    }
    if(KeepStr){
        return(Eq)    
    }else{
        return(formula(Eq))
    }
    
}








# ---------------------------------------
# function: convert class "loadings" to a titled matrix
# NOTE: thanks: https://stackoverflow.com/questions/53825816/convert-a-loadings-object-to-a-dataframe-r
func_Load2Mat <- function( obj ){
    # input
    # 1. obj: a "loadings" instance, usually be pcares$loadings, where pcares is a returned instance by prcomp() or psych::principal()
    # output
    # 1. mat: a numeric matrix (loading matrix), both row & col are named
    # ------------
    df <- data.frame(matrix(as.numeric(obj), attributes(obj)$dim, dimnames=attributes(obj)$dimnames))
    return(df)
}






