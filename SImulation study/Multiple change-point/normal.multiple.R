library(LACPD)
library(parallel)
set.seed(1234)
x <- replicate(500,rnorm(168),simplify = FALSE)

R1 <- mclapply(X=1:length(x), function(i){
  if(i<length(x)) {
    cat(paste(i),",")
    flush.console()
  } else {
    cat(paste(i),"\n")
    flush.console()
  }  
  y <- c(x[[i]][1:40],x[[i]][41:120]+2,x[[i]][121:168]+5)
  
  R.ind <- lacpd_mk(y,m=100,k=c(2:30),adjust = TRUE,method="BY",blow = 0.1
                 ,history = TRUE)
  
  return(R.ind)
  
})

save.image("normal_multiple_new.RData")

######################

allzs1 <- lapply(X=1:length(R1), function(i){
  attr(R1[[i]],"allzs")$`2 - 12`
})
allzs1 <- do.call(rbind,allzs1)
ts.plot(colMeans(allzs1))
R1[[1]]$s[which.max(colMeans(allzs1))]


allps1 <- lapply(X=1:length(R1), function(i){
  attr(R1[[i]],"allps")$`2 - 12`
})
allps1 <- do.call(rbind,allps1)
ts.plot(colMeans(allps1),ylim=c(0,1))
R1[[1]]$s[which.min(colMeans(allps1))]
abline(h=0.05,col=2)
R1[[1]]$s[which(colMeans(allps1)<0.05)]

allmags1 <- lapply(X=1:length(R1), function(i){
  attr(R1[[i]],"allmags")$`2 - 12`
})
allmags1 <- do.call(rbind,allmags1)
ts.plot(colMeans(allmags1))
R1[[1]]$s[which.max(colMeans(allmags1))]


df1 <- data.frame(x=R1[[1]]$s,y=colMeans(allzs1))
df1 <- cbind(df1,ID=rep("Z",135))

df2 <- data.frame(x=R1[[1]]$s,y=colMeans(allps1))
df2 <- cbind(df2,ID=rep("p-value",135))

df3 <- data.frame(x=R1[[1]]$s,y=colMeans(allmags1))
df3 <- cbind(df3,ID=rep("magnitude",135))


library(ggplot2)
png("mult_Z.png",width = 650, height = 650)
ggplot(df1,aes(x=x))+geom_line(aes(y=y),lwd=1.25)+
  facet_wrap(~ID,nrow=1)+
  # geom_vline(data = cpdates, aes(xintercept = v), color ="red", lwd=2) +
  # facet_wrap(~ID,nrow=1)+
  labs(x = "Time",y= "")+
  geom_vline(data = data.frame(xint=40), color="red", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=120), color="red", aes(xintercept = xint),lwd=1.5)+
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=35),
        axis.title=element_text(size=30),strip.text = element_text(size = 35)
        ,legend.text=element_text(size=rel(3)),legend.title=element_text(size=30))+
  scale_x_continuous(breaks = seq(20, 140, by = 20))
dev.off()

R1[[1]]$s[which(df2$y<0.05)]
png("mult_p.png",width = 650, height = 650)
ggplot(df2,aes(x=x))+geom_line(aes(y=y),lwd=1.25)+
  facet_wrap(~ID,nrow=1)+
  labs(x = "Time",y= "")+ylim(c(0,0.3))+
  geom_hline(data = data.frame(yint=0.05), color="brown", aes(yintercept = yint),lwd=1.5)+
  geom_vline(data = data.frame(xint=40), color="red", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=120), color="red",aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=24), color="gold",linetype="dashed", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=59), color="gold",linetype="dashed", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=100), color="gold",linetype="dashed", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=141), color="gold",linetype="dashed", aes(xintercept = xint),lwd=1.5)+
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=35),
        axis.title=element_text(size=30),strip.text = element_text(size = 35)
        ,legend.text=element_text(size=rel(3)),legend.title=element_text(size=30))+
  scale_x_continuous(breaks = seq(20, 140, by = 20))
dev.off()

png("mult_mag.png",width = 650, height = 650)
ggplot(df3,aes(x=x))+geom_line(aes(y=y),lwd=1.25)+
  facet_wrap(~ID,nrow=1)+
  # geom_vline(data = cpdates, aes(xintercept = v), color ="red", lwd=2) +
  # facet_wrap(~ID,nrow=1)+
  labs(x = "Time",y= "")+
  geom_vline(data = data.frame(xint=40), color="red", aes(xintercept = xint),lwd=1.5)+
  geom_vline(data = data.frame(xint=120), color="red", aes(xintercept = xint),lwd=1.5)+
  theme(legend.position="bottom",aspect.ratio=1,axis.text=element_text(size=35),
        axis.title=element_text(size=30),strip.text = element_text(size = 35)
        ,legend.text=element_text(size=rel(3)),legend.title=element_text(size=30))+
  scale_x_continuous(breaks = seq(20, 140, by = 20))+
  scale_y_continuous(breaks = seq(0, 3, by = 0.5))
dev.off()
