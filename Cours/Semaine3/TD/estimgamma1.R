profile <- function(a, n, barX, barlX){
  ll <- -n*a*log(a)+n*a*log(barX)+n*log(gamma(a))-n*(a-1)*barlX +n*a
}

a <- 3
b <- 2
n <- 500
nMC <- 5000

ha1 <- vector(length=nMC)
hb1 <- vector(length=nMC)
ha2 <- vector(length=nMC)
hb2 <- vector(length=nMC)
for (k in (1:nMC)){
  X <- rgamma(n,a,b)
  
  barX <- mean(X)
  hsigma2 <- var(X)*(n-1)/n
  ha1[k]  <- barX^2/hsigma2
  hb1[k] <- ha1[k]/barX
  
  barlX <- mean(log(X))
  r <- optimize(profile,c(0,20),n, barX, barlX)
  ha2[k] <- r$minimum
  hb2[k] <- ha2[k]/barX
}
ha <- data.frame(ha1=ha1,ha2=ha2)
hb <- data.frame(hb1=hb1,hb2=hb2)

par(mfrow=c(1,2))
# par(mfrow=c(1,2))
boxplot(ha)
boxplot(hb)
title(paste0("n = ",n), line=-2, outer=TRUE)
par(mfrow=c(1,1))
