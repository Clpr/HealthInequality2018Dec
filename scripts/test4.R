
# please run main.r first, then run this script

# this script works to plot residual distributions reported in the responses to reviewers


# plot the residuals of FIXED EFFECT MDOELS!
li_Resid <- list(
    illnessratio = residuals(li_ModFix_NHSS$illnessratio),
    illnessday = residuals(li_ModFix_NHSS$illnessday),
    chronicratio = residuals(li_ModFix_NHSS$chronicratio),
    # ----------------
    OUTP1M_RATIO = residuals(li_ModFix_CHARLS$OUTP1M_RATIO),
    CHRONIC_RATIO = residuals(li_ModFix_CHARLS$CHRONIC_RATIO)
)


par( mfrow = c(2,3), omi = c( 0.01,0.01,0.01,0.01 ) )
for(tmpYname in names(li_Resid)){
    hist( li_Resid[[tmpYname]], breaks = 50, freq = FALSE,
          main = "", ylab = "density",
          xlab = paste(sep="","resid(",tmpYname,")" ), font = 4
          )
    lines( density( li_Resid[[tmpYname]] ),
           col = "red"
           )
}















