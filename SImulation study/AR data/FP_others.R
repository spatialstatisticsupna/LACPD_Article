library(LACPD)
library(parallel)
set.seed(1234)
x <- replicate(500,arima.sim(168,model=list(ar=0.5)),simplify = FALSE)
########################################################### other methods
library(strucchange)
FP.str <- unlist(lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }
  y <- x[[i]]
  breakpoints(y~1)$breakpoints[1]
}))
sum(!is.na(FP.str))/length(x)
# [1] 0.526

library(bfast)
FP.bfast <- unlist(lapply(X=1:length(x), function(i){
  y <- x[[i]]
  bfast(ts(y),max.iter = 1,season = "none")$Time[1]
}))
sum(!is.na(FP.bfast))/length(x)
# [1] 0.444

library(ecp)
FP.div <- unlist(lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- x[[i]]
  e.divisive(matrix(y,ncol=1))$order.found[3]
}))
sum(!is.na(FP.div))/length(x)
# [1] 0.654

library(trend)
FP.bu <- lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- x[[i]]
  
  R <- bu.test(y)
  return(list(cp=as.numeric(R$estimate),p=R$p.value))
})
FP.bu <- do.call(rbind,FP.bu)
sum(unlist(FP.bu[,2])<0.05)/length(x)
# [1] 0.372

FP.pettitt <- lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- x[[i]]
  R <- pettitt.test(y)
  return(list(cp=as.numeric(R$estimate),p=R$p.value))
})

FP.pettitt <- do.call(rbind,FP.pettitt)

sum(unlist(FP.pettitt[,2])<0.05)/length(x)
# 0.45
