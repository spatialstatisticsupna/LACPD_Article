library(LACPD)
library(parallel)
set.seed(1234)
x <- replicate(500,arima.sim(168,model = list(ar=0.5)),simplify = FALSE)

R <- mclapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }  
  y <- x[[i]]
  
  out1 <- lacpd_cs(y,m=100,k=c(2:30),adjust = TRUE,method="BY",
                blow = 0.1,history = TRUE)
  
  return(out1)
  
})

save.image("false_AR_cs_BY.RData")

# pss <- matrix(nrow = nrow(attr(R[[1]],"hist")),ncol=length(R))
# for (i in 1:length(R)) {
#   pss[,i] <- attr(R[[i]],"hist")[,4]
# }
# rownames(pss) <- rownames(attr(R[[1]],"hist"))
# FP <- rowMeans(pss<0.05)
# FP[which.min(FP)]
