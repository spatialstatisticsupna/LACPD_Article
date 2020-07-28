library(LACPD)
library(parallel)
set.seed(1234)
x <- replicate(500,rnorm(168),simplify = FALSE)
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
  y <- c(x[[i]][1:40],x[[i]][41:168]+2)
  breakpoints(y~1,breaks = 1)$breakpoints[1]
}))
sum(!is.na(FP.str))/length(x)
# [1] 1
mean(unlist(lapply(X=1:length(x), function(i){
  abs(FP.str[i]-40)
})),na.rm = TRUE)
# [1] 0.596

library(bfast)
FP.bfast <- unlist(lapply(X=1:length(x), function(i){
  y <- c(x[[i]][1:40],x[[i]][41:168]+2)
  bfast(ts(y),max.iter = 1,season = "none",breaks = 1)$Time[1]
}))
sum(!is.na(FP.bfast))/length(x)
# [1] 0.992
round(mean(abs(FP.bfast-40),na.rm = TRUE),3)
# [1] 1.032

library(ecp)
FP.div <- unlist(lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- c(x[[i]][1:40],x[[i]][41:168]+2)
  e.divisive(matrix(y,ncol=1),k=1)$order.found[3]
}))
sum(!is.na(FP.div))/length(x)
# [1] 1
round(mean(abs(FP.div-40),na.rm = TRUE),3)
# [1] 1.29

library(trend)
FP.bu <- lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- c(x[[i]][1:40],x[[i]][41:168]+2)
  
  R <- bu.test(y)
  return(list(cp=as.numeric(R$estimate),p=R$p.value))
})
FP.bu <- do.call(rbind,FP.bu)
sum(unlist(FP.bu[,2])<0.05)/length(x)
# [1] 1
mean(abs(unlist(FP.bu[,1])-40))
# [1] 1.942

FP.pettitt <- lapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  } 
  y <- c(x[[i]][1:40],x[[i]][41:168]+2)
  R <- pettitt.test(y)
  return(list(cp=as.numeric(R$estimate),p=R$p.value))
})

FP.pettitt <- do.call(rbind,FP.pettitt)

sum(unlist(FP.pettitt[,2])<0.05)/length(x)
# 1.00
round(mean(abs(unlist(FP.pettitt[,1])-40)),3)
# [1] 2.551
