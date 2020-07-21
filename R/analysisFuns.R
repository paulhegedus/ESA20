

# function for splitting into trn and val datasets
dualSplit <- function(DT,TRNPROP){
  VALPROP <- 1-TRNPROP
  sumSam <- round(nrow(DT)*TRNPROP) + 
    round(nrow(DT)*VALPROP) 
  dif <- nrow(DT)-sumSam
  if(dif==0){
    subL <- split(DT,sample(c(rep("trn", round(nrow(DT)*TRNPROP)),
                              rep("val", round(nrow(DT)*VALPROP)))))
  }else{
    subL <- split(DT,sample(c(rep("trn", round(nrow(DT)*TRNPROP)+dif),
                              rep("val", round(nrow(DT)*VALPROP)))))
  }
  return(subL)
}




plotVal <- function(df, respvar, predvar, field_name, year, MOD, SAVE) {
  respcol <- grep(respvar, names(df))
  predcol <- grep(predvar, names(df))
  
  if (grepl("cent", respvar)) {
    yMIN <- DescTools::RoundTo(min(df[,respcol],na.rm=T),5,floor)
  } else {
    yMIN <- 0
  }
  yMAX <- DescTools::RoundTo(max(df[,respcol],na.rm=T),5,ceiling)
  ySTEP <- (DescTools::RoundTo(max(df[,respcol],na.rm=T),5,ceiling)-
              DescTools::RoundTo(min(df[,respcol],na.rm=T),5,floor))/10
  xMIN <- DescTools::RoundTo(min(df$aa_n,na.rm=T),5,floor)
  xMAX <- DescTools::RoundTo(max(df$aa_n,na.rm=T),5,ceiling)
  xSTEP <- (DescTools::RoundTo(max(df$aa_n,na.rm=T),5,ceiling)-
              DescTools::RoundTo(min(df$aa_n,na.rm=T),5,floor))/10
  
  lab <- ifelse(grepl("yld", respvar),
                "Yield (bu/ac)",
                ifelse(grepl("pro", respvar),
                       "Grain Protein Content (%)",
                       "Net-return ($/ac)"))
  colr <-  ifelse(grepl("yld", respvar),
                  "red",
                  ifelse(grepl("pro", respvar),
                         "cyan",
                         "green"))
  if (grepl("cent", respvar)) {
    lab <- paste0("Centered ", lab)
  }
  
  p <- ggplot() +
    geom_point(data = df, aes(x = aa_n, y = get(respvar, df)), col = "black") + 
    labs(y = lab, x = "As-Applied Nitrogen (lbs N/ac)") + 
    ggtitle(paste0(field_name, " ", year),
            subtitle = paste0("AIC = ", round(AIC(MOD), 4))) +
    geom_point(data = df, aes(x = aa_n, y =  get(predvar, df)), col = colr) +
    scale_color_manual(name = "", 
                       values=c("black", colr),
                       labels = c("Observed",
                                  "Predicted")) +
    scale_y_continuous(limits=c(yMIN,yMAX), 
                       breaks=seq(yMIN,yMAX,ySTEP)) +
    scale_x_continuous(limits=c(xMIN,xMAX), 
                       breaks=seq(xMIN,xMAX,xSTEP)) +
    theme_bw() 
  #print(p)
  if (SAVE) {
    ggsave(paste0("results/", field_name, "/diagnostics/", 
                  field_name, "_", year, "_", respvar, "_pred&ObsVsN.png"),
           plot = p,
           device = "png",
           scale = 1,
           width = 7.5, 
           height = 5,
           units = "in")
  }
  return(p)
}

plotObsVPreds <- function(df, respvar, predvar, field_name, year, SAVE) {
  respcol <- grep(respvar, names(df))
  predcol <- grep(predvar, names(df))
  MAX <- 
    ifelse(
      max(df[,respcol],na.rm=T)>max(df[,predcol],na.rm=T),
      DescTools::RoundTo(max(df[,respcol],na.rm=T),5,ceiling),
      DescTools::RoundTo(max(df[,predcol],na.rm=T),5,ceiling)
    )
  MIN <- 
    ifelse(
      min(df[,respcol],na.rm=T)<min(df[,predcol],na.rm=T),
      DescTools::RoundTo(min(df[,respcol],na.rm=T),5,floor),
      DescTools::RoundTo(min(df[,predcol],na.rm=T),5,floor))
  
  colr <-  ifelse(grepl("yld", respvar),
                  "red",
                  ifelse(grepl("pro", respvar),
                         "cyan",
                         "green"))
  lab <- ifelse(grepl("yld", respvar),
                "Yield (bu/ac)",
                ifelse(grepl("pro", respvar),
                       "Grain Protein Content (%)",
                       "Net-return ($/ac)"))
  
  p <- ggplot(data=df) + 
    geom_point(aes(x=get(respvar, df)), y=get(predvar, df)) +
    geom_abline(intercept=0,
                slope=1,
                color=colr) +
    labs(x=paste0("Observed ", lab),
         y=paste0("Predicted ", lab)) +
    scale_y_continuous(limits=c(MIN,MAX),
                       breaks=seq(MIN,
                                  MAX,
                                  (MAX-MIN)/10)) +
    scale_x_continuous(limits=c(MIN,MAX),
                       breaks=seq(MIN,
                                  MAX,
                                  (MAX-MIN)/10)) +
    theme_bw() +
    ggtitle( paste0("Predicted vs. Observed ", lab),
      subtitle = paste0(
        "Line = 1:1, RMSE = ",
        suppressWarnings(
          round(Metrics::rmse(
              na.omit(get(respvar, df)),
              na.omit(get(predvar, df))),
              4)), "   ", year))
  # print(p)
  if (SAVE) {
    ggsave(paste0("results/", field_name, "/diagnostics/", 
                  field_name, "_", year, "_", respvar, "_predVsObs.png"),
           plot = p,
           device = "png",
           scale = 1,
           width = 7.5, 
           height = 5,
           units = "in")
  }
  return(p)
}



sensitivity <- function(sensTable, respvar, dat, MOD) {
  #browser()
  
  predcol <- grep(paste0("^gam_pred_", respvar,"$"), names(dat))
  sensTable[1, 3] <- mean(dat[, predcol])
  for (i in 2:nrow(sensTable)) {
    dtemp <- dat
    adjCol <- grep(paste0("^", sensTable$Parameter[i], "$"), names(dtemp))
    dtemp[, adjCol] <- dtemp[, adjCol] + dtemp[, adjCol] * (1/as.numeric(sensTable$Adjustment[i]))
    
    #model <- mgcv::gam(as.formula(MOD$formula), data = dtemp)
    preds <- mgcv::predict.bam(MOD, dtemp)
    sensTable[i, 3] <- mean(preds)
    
    sensTable$Sensitivity.value[i] <-
      abs((((as.numeric(sensTable[1,3])-as.numeric(sensTable[i,3]))/
              as.numeric(sensTable[1,3]))))/.1
  }
  sensTable$Elasticity.value <-
    abs(as.numeric(sensTable$Sensitivity.value))/
    max(as.numeric(sensTable$Sensitivity.value))
  return(sensTable)
}

