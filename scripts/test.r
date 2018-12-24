Data <- df_CHARLS
Xcandidates <- setdiff( envCHARLS$Xcandidate, "" )

# 将除了核心以外的所有变量做主成分降维
PcN <- 17
res <- prcomp( Data[,envCHARLS$Xcandidate], center = TRUE, scale = TRUE, retx = TRUE, rank.= PcN )
summary(res)
# -----------
tmpeq <- formula(paste(sep="", "OUTP1M_RATIO ~ AVGINDIINCOME_TOTAL + AVGEDU + ", paste(collapse = "+",sep="", "PC",1:PcN ) ))

# ------------
Data2 <- data.frame(  res$x, Data )
pData2 <- pdata.frame( Data2, index = c( envCHARLS$flagCity, envCHARLS$flagT ) )

# -------------------
mod <- plm::plm( tmpeq , pData2, effect = "individual", model = "within" )
mod2 <- plm::plm( tmpeq , pData2, effect = "individual", model = "random" )
mod3 <- plm::plm( tmpeq , pData2, effect = "twoways", model = "within" )
mod4 <- gls( tmpeq, pData2 )

# ----------------
summary(mod, vcov = vcovHC(mod) )
summary(mod2, vcov = vcovHC(mod2) )
summary(mod3, vcov = vcovHC(mod3) )    
summary(mod4)

# -----------



# View(cor(mod$model))




# ====================================================
# ====================================================


Data <- df_NHSS
Xcandidates <- setdiff( envNHSS$Xcandidate, "" )

# 将除了核心以外的所有变量做主成分降维
PcN <- 6
res <- prcomp( Data[,envNHSS$Xcandidate], center = TRUE, scale = TRUE, retx = TRUE, rank.= PcN )
summary(res)
# -----------
tmpeq <- formula(paste(sep="", "illnessratio ~ income + edu + ", paste(collapse = "+",sep="", "PC",1:PcN ) ))

# ------------
Data2 <- data.frame(  res$x, Data )
pData2 <- pdata.frame( Data2, index = c( envNHSS$flagCity, envNHSS$flagT ) )

# -------------------
mod <- plm::plm( tmpeq , pData2, effect = "individual", model = "within" )
mod2 <- plm::plm( tmpeq , pData2, effect = "individual", model = "random" )
mod3 <- plm::plm( tmpeq , pData2, effect = "twoways", model = "within" )
mod4 <- gls( tmpeq, pData2 )

# ----------------
summary(mod, vcov = vcovHC(mod) )
summary(mod2, vcov = vcovHC(mod2) )
summary(mod3, vcov = vcovHC(mod3) )    
summary(mod4)









