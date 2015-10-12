ozone <- read.table("ozone.txt",header=T,sep=" ")
library("scatterplot3d")
scatterplot3d(ozone[,"T12"],ozone[,"Vx12"],ozone[,"maxO3"],type="h", pch=16,box=FALSE,xlab="T12",ylab="Vx12",zlab="maxO3")
modele3 <- lm(maxO3~T12+Vx12+Ne12,data=ozone)
resume3 <- summary(modele3)
coef3 <- coef(resume3)
IC3 <- confint(modele3,level=0.95)
IC3= t(IC3)
library(ellipse)
par(mfrow=c(3,2))
for(i in 1:3){
  for(j in (i+1):4){
    plot(ellipse(modele3,c(i,j),level=0.95),type="l", xlab=paste("beta",i,sep=""),ylab=paste("beta",j,sep=""))
    points(coef(modele3)[i], coef(modele3)[j],pch=3)
    lines(c(IC3[1,i],IC3[1,i],IC3[2,i],IC3[2,i],IC3[1,i]), c(IC3[1,j],IC3[2,j],IC3[2,j],IC3[1,j],IC3[1,j]),lty=2)
    }}
