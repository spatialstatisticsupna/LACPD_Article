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

############################################## 
############################################## plots
############################################## 
############################# data preparation

allzs1 <- lapply(X=1:length(R1), function(i){
  attr(R1[[i]],"allzs")$`2 - 12`
})
allzs1 <- do.call(rbind,allzs1)
ts.plot(colMeans(allzs1))
R1[[1]]$s[which.max(colMeans(allzs1))]
# sd40 <- apply(allzs1, 2,sd)

allzs2 <- lapply(X=1:length(R2), function(i){
  attr(R2[[i]],"allzs")$`2 - 12`
})
allzs2 <- do.call(rbind,allzs2)
ts.plot(colMeans(allzs2))
R2[[1]]$s[which.max(colMeans(allzs2))]
# sd80 <- apply(allzs2, 2,sd)


allzs3 <- lapply(X=1:length(R3), function(i){
  attr(R3[[i]],"allzs")$`2 - 12`
})
allzs3 <- do.call(rbind,allzs3)
ts.plot(colMeans(allzs3))
R3[[1]]$s[which.max(colMeans(allzs3))]
# sd120 <- apply(allzs3, 2,sd)


allps1 <- lapply(X=1:length(R1), function(i){
  attr(R1[[i]],"allps")$`2 - 12`
})
allps1 <- do.call(rbind,allps1)
ts.plot(colMeans(allps1))
R1[[1]]$s[which.min(colMeans(allps1))]
abline(h=0.05,col=2)
R1[[1]]$s[which(colMeans(allps1)<0.05)]

allps2 <- lapply(X=1:length(R2), function(i){
  attr(R2[[i]],"allps")$`2 - 12`
})
allps2 <- do.call(rbind,allps2)
ts.plot(colMeans(allps2))
R2[[1]]$s[which.min(colMeans(allps2))]
abline(h=0.05,col=2)
R2[[1]]$s[which(colMeans(allps2)<0.05)]


allps3 <- lapply(X=1:length(R3), function(i){
  attr(R3[[i]],"allps")$`2 - 12`
})
allps3 <- do.call(rbind,allps3)
ts.plot(colMeans(allps3))
R3[[1]]$s[which.min(colMeans(allps3))]
abline(h=0.05,col=2)
R3[[1]]$s[which(colMeans(allps3)<0.05)]


S <- R1[[1]]$s

df1.p <- data.frame(x=S,y=as.numeric(colMeans(allps1)))
df1.p <- cbind(df1.p,ID=rep("40",135))

df2.p <- data.frame(x=S,y=as.numeric(colMeans(allps2)))
df2.p <- cbind(df2.p,ID=rep("80",135))

df3.p <- data.frame(x=S,y=as.numeric(colMeans(allps3)))
df3.p <- cbind(df3.p,ID=rep("120",135))

df.p <- rbind(df1.p,df2.p,df3.p)
type <- rep("p-value",nrow(df.p))
df.p <- cbind(df.p,type)


df1 <- data.frame(x=S,y=as.numeric(colMeans(allzs1)))
df1 <- cbind(df1,ID=rep("40",135))
# df1 <- cbind(df1,up=df1$y+sd40,low=df1$y-sd40)

df2 <- data.frame(x=S,y=as.numeric(colMeans(allzs2)))
df2 <- cbind(df2,ID=rep("80",135))
# df2 <- cbind(df2,up=df2$y+sd80,low=df2$y-sd80)

df3 <- data.frame(x=S,y=as.numeric(colMeans(allzs3)))
df3 <- cbind(df3,ID=rep("120",135))
# df3 <- cbind(df3,up=df3$y+sd120,low=df3$y-sd120)

df <- rbind(df1,df2,df3)
type <- rep("Z",nrow(df))
df <- cbind(df,type)

S[which.min(colMeans(allps1))]
S[which.min(colMeans(allps2))]
S[which.min(colMeans(allps3))]

S[which(colMeans(allps1)<0.2)]
S[which(colMeans(allps2)<0.2)]
S[which(colMeans(allps3)<0.2)]

dpz <- rbind(df,df.p)


library(ggplot2)

png("normalshift-mk-BY-pz.png",width = 1600, height = 1600)
ggplot(dpz,aes(x=x))+geom_line(aes(y=y),lwd=2)+
  facet_grid(type ~ ID, scales = "free")+
  # geom_line(data=data.frame(x=df1[,1],y=df1[,4],type="Z",ID="40"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  # geom_line(data=data.frame(x=df1[,1],y=df1[,5],type="Z",ID="40"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  # geom_line(data=data.frame(x=df2[,1],y=df2[,4],type="Z",ID="80"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  # geom_line(data=data.frame(x=df2[,1],y=df2[,5],type="Z",ID="80"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  # geom_line(data=data.frame(x=df3[,1],y=df3[,4],type="Z",ID="120"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  # geom_line(data=data.frame(x=df3[,1],y=df3[,5],type="Z",ID="120"),aes(x=x, y=y), stat="identity", colour = "blue",lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=40,type="Z",ID="40"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=80,type="Z",ID="80"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=120,type="Z",ID="120"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=40,type="p-value",ID="40"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=80,type="p-value",ID="80"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=120,type="p-value",ID="120"), color=2, aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=34,type="p-value",ID="40"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=72,type="p-value",ID="80"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=114,type="p-value",ID="120"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=47,type="p-value",ID="40"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=88,type="p-value",ID="80"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_vline(data = data.frame(xint=127,type="p-value",ID="120"), color="gold", aes(xintercept = xint),lwd=1.5,linetype="dashed")+
  geom_hline(data = data.frame(yint=0.05,type="p-value"), color="brown", aes(yintercept = yint),lwd=1.5)+
  labs(x = "Time",y= "")+
  theme(legend.position="bottom",aspect.ratio=1.5,axis.text=element_text(size=45),
        axis.title=element_text(size=45),strip.text = element_text(size = 45)
        ,legend.text=element_text(size=rel(3)),legend.title=element_text(size=45))
dev.off()

