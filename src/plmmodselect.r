# <-- encoding UTF-8 -->
# Empirical study on NHSS dataset (county level)
# -------------------------------------
## DOC STRING
#   does model selection based on AIC, BIC for plm models
# 
# Tianhao Zhao (GitHub: Clpr)
# Dec 2018
# -------------------------------------

Data <- dfp_CHARLS
# mod <- plm::plm( OUTP1M_RATIO ~ AVGINDIINCOME_TOTAL + AVGEDU, dfp_CHARLS, effect = "individual", model = "within" )
# mod <- plm::plm( OUTP1M_RATIO ~ AVGINDIINCOME_TOTAL + AVGEDU, dfp_CHARLS, effect = "individual", model = "random" )
Yname <- "OUTP1M_RATIO"; Xkeep <- c("AVGINDIINCOME_TOTAL","AVGEDU")
Xcandidates <- envCHARLS$Xcandidate
Effect = "individual"; Model = "within"
criteria <- "AIC"
Intercept <- TRUE

# ----------------------------------
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




# -----------------------------------
# function: for plm() class, select a mod with least information criteria value (F stat), Forward
# NOTE: dispatched method
# NOTE: supports start from a specific specification, allows keeping specific independent variables
func_plmSelectForwardFstat <- function( Data, Yname, Xkeep, Xcandidates, Effect = "individual", Model = "within", Intercept = TRUE ){
    # 1. construct basic specification (string), estimate a benchmark specification
    EqBase <- func_GetEq(Yname,Xkeep,Intercept = Intercept, KeepStr = TRUE )
    ModBase <- plm::plm( formula(EqBase), Data, effect = Effect, model = Model )
    # 2. measures
    K <- length(Xcandidates)  # max vars to add
    Eqcurrent < - EqBase   # current specification
    Xcurrent <- Xcandidates  # a copy, to store current X names (not includes Xkeep)
    Fmax <- summary(ModBase, vcov = vcovHC(ModBase) )$fstatistic$statistic[1,1]  # current maximum F stat
    # 3. loop
    idxk <- 1  # counter
    while(idxk <= K){
        tmpEq <- Eqcurrent   # get current specifications (in string)
        tmpX <- Xcurrent  # get what candidates can be added
        tmpK <- length(tmpX)  # how many candidates can be added
    }
    
    
    print("fuck")
    
    
    return(NULL)
}














# -------------------------
# function: plm model selection based on AIC OR BIC
# NOTE: it is a portal function; then dispatched to diff methods
func_plmSelect <- function( Data, Yname, Xkeep, Xcandidates, Test = "F", Effect = "individual", Model = "within", Intercept = TRUE, Direction = "forward" ){
    # input:
    # 1. Data:          a pdata.frame, containing all variables used
    # 2. Yname:         the dependent's name
    # 3. Xkeep:         a character vector of which Xs to keep in & start from 
    # 3. Xcandidates:   a character of other possible variables to enter into the specification
    # 4. criteria:      what kind of information criteria to use, AIC by default, select on in c("AIC","BIC")
    # 5. Effect:        effect for plm()
    # 6. Model:         model type for plm()
    # 7. Intercept:     bool, if to keep intercept in the specification, TURE by default
    # 8. Direction:     direction, in c("forward","backward","stepwise")
    # output:
    # 1. Selectmod:     a plm object, the selected model
    # dependency:
    # 1. func_GetEq()
    # ------------------------------
    # 0. type assertion
    if( !("pdata.frame" %in% class(Data)) ){ stop("pdata.frame Data required") }
    if( !("character" %in% class(Yname)) ){ stop("character Yname required") }  
    if( !("character" %in% class(Xkeep)) ){ stop("character Xkeep required") }  
    if( !("character" %in% class(Xcandidates)) ){ stop("character Xcandidates required") }  
    if( !(criteria %in% c("AIC","BIC") ) ){ stop("not supported criteria") } 
    # 1. dispatch
    # if(class)
    
    
    return(NULL)
}






