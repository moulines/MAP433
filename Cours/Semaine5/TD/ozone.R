# data
rm(list=ls())
load("ozone.Rdata")
dim(ozone)
names(ozone)

# linear regression
reg1 <- lm(O3~.,data=ozone)
names(reg1)
reg1$coefficients

# Inspection of residuals
hist(reg1$residuals)

qqnorm(reg1$residuals); qqline(reg1$residuals, col=2, lty=2)

# Renormalisation
X<-as.matrix(ozone[,-1])
P<-X%*%solve(t(X)%*%X)%*%t(X)
renorm<-(1-diag(P))**(-1/2)*reg1$residuals
qqnorm(renorm); qqline(renorm, col=2, lty=2)

# homoscedastique et autocorrelation
plot(reg1$fitted.values,abs(reg1$residuals), col=2); lines(lowess(reg1$fitted.values,abs(reg1$residuals),f=0.7),lty=2)

plot(reg1$residuals,xlab="day",col=2); lines(lowess(reg1$residuals,f=0.7),lty=2)

# Results
summary(reg1)


# Reduced model
reg2 <- lm(O3~Ne12+O3v,data=ozone)

summary(reg2)

par(mfrow=c(2,2))
hist(reg2$residual)
qqnorm(reg2$residuals); qqline(reg2$residuals, col=2, lty=2)
plot(reg2$fitted.values,abs(reg2$residuals), col=2); lines(lowess(reg2$fitted.values,abs(reg2$residuals),f=0.7),lty=2)
plot(reg2$residuals,xlab="day",col=2); lines(lowess(reg2$residuals,f=0.7),lty=2)


# Intermediate model
reg3 <- lm(O3~T15+Ne12+Vx+O3v,data=ozone)
summary(reg3)

# Model selection
library(leaps)
sel<-regsubsets(O3~.,method="exhaustive",data=ozone)
par(mfrow=c(1,2))
plot(summary(sel)$bic,xlab="dimension",col=2)
plot(sel)
# T15, Ne12, Vx, O3v

# Linear modeling: partial regression
PartialRes <- lm(O3~Ne12+Vx+O3v,data=ozone)$residuals
PartialT15 <- lm(T15~Ne12+Vx+O3v,data=ozone)$residuals
plot(PartialT15,PartialRes, col=2); lines(lowess(PartialT15,PartialRes,f=0.7), lty=2)

plot(O3~T15,data=ozone,col=2); lines(lowess(ozone[c("T15","O3")],f=0.7), lty=2)

PartialRes <- lm(O3~T15+Vx+O3v,data=ozone)$residuals
PartialNe12 <- lm(Ne12~T15+Vx+O3v,data=ozone)$residuals
plot(PartialNe12,PartialRes, col=2); lines(lowess(PartialNe12,PartialRes,f=0.7), lty=2)

PartialRes <- lm(O3~T15+Ne12+Vx,data=ozone)$residuals
PartialO3v <- lm(O3v~T15+Ne12+Vx,data=ozone)$residuals
plot(PartialO3v,PartialRes, col=2); lines(lowess(PartialO3v,PartialRes,f=0.7), lty=2)


ozone["T15"] <- pmax(ozone["T15"]-20,0)




