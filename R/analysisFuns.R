

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