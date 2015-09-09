
a <- 3
b <- 2
alpha=0.05

read.data <- TRUE
# read.data <- FALSE

if (read.data==FALSE){
  n <- 500
  X <- rgamma(n,a,b)
  write.table(round(X,3),"gamma500_data.txt",
              quote=FALSE,row.names=FALSE,col.names=FALSE)
}else{
#   X <- read.table("gamma20_data.txt")
    X <- read.table("gamma500_data.txt")
  X <- X[,1]
  n <- length(X)
}

# Estimateur de (a,b) par la méthode du maximum de vraisemblance
profile <- function(a, n, barX, barlX){
  ll <- -n*a*log(a)+n*a*log(barX)+n*log(gamma(a))-n*(a-1)*barlX +n*a
}

barX <- mean(X)
barlX <- mean(log(X))
r <- optimize(profile,c(0,20),n, barX, barlX)
ha <- r$minimum
hb <- ha/barX


# Matrice d'Information de Fisher
I <- matrix(c(trigamma(ha), -1/hb, -1/hb, ha/hb^2), ncol=2)
In <- n*I
Cn <- round(solve(In),4)


# Monte Carlo (tirages dans la loi)
n.mc <- 5000
ha.mc <- vector(length=n.mc)
hb.mc <- vector(length=n.mc)
for (k in (1:n.mc)){
  X.mc <- sample(X, size=n, replace = TRUE)
  barX.mc <- mean(X.mc)  
  barlX.mc <- mean(log(X.mc))
  r <- optimize(profile,c(0,20),n, barX.mc, barlX.mc)
  ha.mc[k] <- r$minimum
  hb.mc[k] <- ha.mc[k]/barX.mc
}

# Bootstrap (tirages avec remise)
n.b <- 5000
ha.b <- vector(length=n.b)
hb.b <- vector(length=n.b)
for (k in (1:n.b)){
  X.b <- sample(X, size=n, replace = TRUE)
  barX.b <- mean(X.b)  
  barlX.b <- mean(log(X.b))
  r <- optimize(profile,c(0,20),n, barX.b, barlX.b)
  ha.b[k] <- r$minimum
  hb.b[k] <- ha.b[k]/barX.b
}

# Estimateurs et leurs variances
cat("\nEstimateurs du maximum de vraisemblance de a et b :")
cat(paste0("\n","ha = ",round(ha,2),"\n","hb = ",round(hb,2),"\n"))

cat(paste0("\n","var(ha,hb) : \n"))
Chab <- data.frame(Cn,row.names=c("ha","hb"))
names(Chab) <- c("ha","hb")
cat("\nTCL:\n")
print(Chab)
cat("\n\nMonte Carlo:\n")
print(round(var(data.frame(ha=ha.mc,hb=hb.mc)),4))
cat("\n\nBootstrap:\n")
print(round(var(data.frame(ha=ha.b,hb=hb.b)),4))

# Intervalles de confiance
sa <- sqrt(Cn[1,1])
sb <- sqrt(Cn[2,2])
cat(paste0("\nIntervalles de confiance de niveau ",1-alpha," :\n"))
cat("\nTCL:\n")
cat("a ",round(c(ha + qnorm(alpha/2)*sa, ha + qnorm(1-alpha/2)*sa),2),"\n")
cat("b ",round(c(hb + qnorm(alpha/2)*sb, hb + qnorm(1-alpha/2)*sb),2),"\n")
cat("\nMonte Carlo:\n")
cat("a ",round(quantile(ha.mc,c(alpha/2,1-alpha/2)),2),"\n")
cat("b ",round(quantile(hb.mc,c(alpha/2,1-alpha/2)),2),"\n")
cat("\nBootstrap:\n")
cat("a ",round(quantile(ha.b,c(alpha/2,1-alpha/2)),2),"\n")
cat("b ",round(quantile(hb.b,c(alpha/2,1-alpha/2)),2),"\n")


#Densités de probabilité
nx <- 100
xa1 <- qnorm(0.0001,mean=ha,sd=sa)
xa2 <- qnorm(0.9999,mean=ha,sd=sa)
xa <- seq(xa1,xa2,length.out=nx)
da1 <- dnorm(xa,mean=ha,sd=sa)
da2 <- density(ha.mc,from=xa1, to=xa2, n=nx)
da3 <- density(ha.b,from=xa1, to=xa2, n=nx)
da <- data.frame(a=xa,TCL=da1,MonteCarlo=da2$y,Bootstrap=da3$y)

xb1 <- qnorm(0.001,mean=hb,sd=sb)
xb2 <- qnorm(0.999,mean=hb,sd=sb)
xb <- seq(xb1,xb2,length.out=nx)
db1 <- dnorm(xb,mean=hb,sd=sb)
db2 <- density(hb.mc,from=xb1, to=xb2, n=nx)
db3 <- density(hb.b,from=xb1, to=xb2, n=nx)
db <- data.frame(b=xb,TCL=db1,MonteCarlo=db2$y,Bootstrap=db3$y)


require(reshape)
da <- melt(da ,  id.vars = 'a')
names(da) <- c("a","methode","densité")
db <- melt(db ,  id.vars = 'b')
names(db) <- c("b","methode","densité")

require(ggplot2)
ggplot <- function(...) {ggplot2::ggplot(...) + theme_bw() +
                           theme(plot.background = element_rect(fill=rgb(1,1,1))) }

pla <- ggplot(da, aes(a,densité)) + geom_line(aes(colour = methode))
plb <- ggplot(db, aes(b,densité)) + geom_line(aes(colour = methode))

require(gridExtra)
grid.arrange(pla,plb)

