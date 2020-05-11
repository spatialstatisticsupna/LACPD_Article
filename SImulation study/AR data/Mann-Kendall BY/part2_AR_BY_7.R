library(Scp)
library(parallel)
set.seed(1234)
x <- replicate(500,arima.sim(168,model = list(ar=0.5)),simplify = FALSE)

R2 <- mclapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }  
  y <- c(x[[i]][1:80],x[[i]][81:168]+3)
  
  out1 <- Re_mk(y,m=100,k=c(2:30),adjust = TRUE,method="BY",
                blow = 0.1,history = TRUE)
  
  return(out1)
  
})

save.image("part2_AR_BY_7.RData")


cps <- matrix(nrow = nrow(attr(R2[[1]],"hist")),ncol=length(R2))
for (i in 1:length(R2)) {
  cps[,i] <- attr(R2[[i]],"hist")[,1]
}
rownames(cps) <- rownames(attr(R2[[1]],"hist"))
MAE <- rowMeans(abs(cps-80))
MAE[which.min(MAE)]

pss <- matrix(nrow = nrow(attr(R2[[1]],"hist")),ncol=length(R2))
for (i in 1:length(R2)) {
  pss[,i] <- attr(R2[[i]],"hist")[,4]
}
rownames(pss) <- rownames(attr(R2[[1]],"hist"))
power <- rowMeans(pss<0.05)
power[which.max(power)]
