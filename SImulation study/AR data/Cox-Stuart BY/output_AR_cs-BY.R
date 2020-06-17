# load output

load("./part1_AR_cs_BY.RData")
load("./part2_AR_cs_BY.RData")
load("./part3_AR_cs_BY.RData")
load("./false_AR_cs_BY.RData")


######### extract MAE and power

# 40
cps1 <- matrix(nrow = nrow(attr(R1[[1]],"hist")),ncol=length(R1))
for (i in 1:length(R1)) {
  cps1[,i] <- attr(R1[[i]],"hist")[,1]
}
rownames(cps1) <- rownames(attr(R1[[1]],"hist"))
MAE1 <- rowMeans(abs(cps1-40))
# MAE3[which.min(MAE3)]

pss1 <- matrix(nrow = nrow(attr(R1[[1]],"hist")),ncol=length(R1))
for (i in 1:length(R1)) {
  pss1[,i] <- attr(R1[[i]],"hist")[,4]
}
rownames(pss1) <- rownames(attr(R1[[1]],"hist"))
power1 <- rowMeans(pss1<0.05)
# power1[which.max(power1)]

d1 <- cbind(MAE=MAE1,power=power1)
head(d1)


# 80
cps2 <- matrix(nrow = nrow(attr(R2[[1]],"hist")),ncol=length(R2))
for (i in 1:length(R2)) {
  cps2[,i] <- attr(R2[[i]],"hist")[,1]
}
rownames(cps2) <- rownames(attr(R2[[1]],"hist"))
MAE2 <- rowMeans(abs(cps2-80))
# MAE2[which.min(MAE2)]

pss2 <- matrix(nrow = nrow(attr(R2[[1]],"hist")),ncol=length(R2))
for (i in 1:length(R2)) {
  pss2[,i] <- attr(R2[[i]],"hist")[,4]
}
rownames(pss2) <- rownames(attr(R2[[1]],"hist"))
power2 <- rowMeans(pss2<0.05)
# power2[which.max(power2)]

d2 <- cbind(MAE=MAE2,power=power2)
head(d2)

# 120
cps3 <- matrix(nrow = nrow(attr(R3[[1]],"hist")),ncol=length(R3))
for (i in 1:length(R3)) {
  cps3[,i] <- attr(R3[[i]],"hist")[,1]
}
rownames(cps3) <- rownames(attr(R3[[1]],"hist"))
MAE3 <- rowMeans(abs(cps3-120))
# MAE3[which.min(MAE3)]

pss3 <- matrix(nrow = nrow(attr(R3[[1]],"hist")),ncol=length(R3))
for (i in 1:length(R3)) {
  pss3[,i] <- attr(R3[[i]],"hist")[,4]
}
rownames(pss3) <- rownames(attr(R3[[1]],"hist"))
power3 <- rowMeans(pss3<0.05)
# power3[which.max(power3)]

d3 <- cbind(MAE=MAE3,power=power3)
head(d3)

# False positives
pss.fp <- matrix(nrow = nrow(attr(R[[1]],"hist")),ncol=length(R))
for (i in 1:length(R)) {
  pss.fp[,i] <- attr(R[[i]],"hist")[,4]
}
rownames(pss.fp) <- rownames(attr(R[[1]],"hist"))
FP <- rowMeans(pss.fp<0.05)
# FP[which.min(FP)]


dd <- cbind(d1,d2,d3,FP)
head(dd)

seqq <- c(seq(1,28,by=3),seq(31,39,by=3),seq(57,65,by=3))
dd[seqq,]

library(xtable)
xtable(dd[seqq,])

