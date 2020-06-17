library(LACPD)
library(parallel)
set.seed(1234)
x <- replicate(500,arima.sim(168,model = list(ar=0.5)),simplify = FALSE)

R3 <- mclapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }  
  y <- c(x[[i]][1:120],x[[i]][121:168]+2)
  
  out1 <- lacpd_cs(y,m=100,k=c(2:30),adjust = TRUE,method="BY",
                blow = 0.1,history = TRUE)
  
  return(out1)
  
})

save.image("part3_AR_cs_BY.RData")


# cps <- matrix(nrow = nrow(attr(R3[[1]],"hist")),ncol=length(R3))
# for (i in 1:length(R3)) {
#   cps[,i] <- attr(R3[[i]],"hist")[,1]
# }
# rownames(cps) <- rownames(attr(R3[[1]],"hist"))
# MAE <- rowMeans(abs(cps-120))
# MAE[which.min(MAE)]
# 
# pss <- matrix(nrow = nrow(attr(R3[[1]],"hist")),ncol=length(R3))
# for (i in 1:length(R3)) {
#   pss[,i] <- attr(R3[[i]],"hist")[,4]
# }
# rownames(pss) <- rownames(attr(R3[[1]],"hist"))
# power <- rowMeans(pss<0.05)
# power[which.max(power)]
