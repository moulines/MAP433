library(ggplot2)
library(MASS)

ggplotmlx <- function(...) {ggplot2::ggplot(...) + theme_bw() +
                              theme(plot.background = element_rect(fill=rgb(1,1,1))) }

shinyServer(function(input, output) {
  d <- read.csv(file="../polydata1.txt", header=TRUE, sep="\t", quote="\"")
  d$y <- d$y+10
  y <- d$y
  time <- d$time
  p.names <- "c_0"
  for (k in (1:length(y)))
    p.names <- c(p.names,paste0("c_",k))
  nc <- 200
  n <- length(time)
  
  r <- reactive({
    if (input$opf==FALSE){
      tc <- seq(min(d$t),max(d$t),length=nc)
    }else{
      tc <- seq(0,max(d$t)+4,length=nc)
    }
    f <- data.frame(time=tc)
    r=list()
    dg <- as.numeric(input$degree)
    
    if (input$widget == 'list')
      lambda <- as.numeric(input$lambda1)
    else
      lambda <- as.numeric(input$lambda2)
    
    X <- cbind(rep(1,n))
    Xc <- cbind(rep(1,nc))
    if (dg>0){
      for (k in (1:dg)){
        X <- cbind(X,time^k)
        Xc <- cbind(Xc,tc^k)
      }
    }
    dX <- diag(t(X)%*%X)
    dX[1] <- 1
    dX <- diag(1/sqrt(dX))
    X <- X%*%dX
    Xc <- Xc%*%dX
    
    res <- list()
    
    if (dg==0){
      res$coefficients <- mean(y)    
    }else if (dg==1){
      r <- lm(y~poly(time,dg,raw=TRUE))
      r.opt <- optim(r$coefficients,errpred,y=d$y,X=X,lambda=lambda)
      res$coefficients <- r.opt$par
    }else{
       a <- lm.ridge(y~X[,2:(dg+1)],lambda=lambda)
       res$coefficients <- as.vector(coef(a))
      
#             yg <- c(y,rep(0,dg))
#             G=cbind(rep(0,dg),diag(dg)*sqrt(lambda))
#             XG=rbind(X,G)
#             res<-lm(yg~0+XG[,1:(dg+1)])
    } 
    res$fitted.values <- X %*% res$coefficients
    f$f <- Xc %*% res$coefficients
    
    d$ypred <- res$fitted.values
    d$e <- d$y - d$ypred
    S1 <- sum(d$e^2)
    M=diag(diag(t(X)%*%X))/n
    M[1,1]=0
    S2 <- t(res$coefficients)%*%M%*%res$coefficients
    #     S2 <- sum(res$coefficients[2:(dg+1)]^2)
    S3 <- S1 + lambda*S2
    r$sse=data.frame(result=c(S1,S2,S3))
    row.names(r$sse) <- c("SSE" , "SSP" , "Stot")
    names(r$sse) <- ""
    
    r$result <- data.frame(estimate=res$coefficients)
    row.names(r$result) <- p.names[1:(dg+1)]
    r$y <- d
    r$f <- f
    r
  })
  
  output$plot1 <- renderPlot({
    r=r()
    cl0 <- "#CC0033"
    sz0 <- 0.75
    cl1 <- "#6666CC"
    sz1 <- 3.5
    plerr=1
    pl <- ggplotmlx(data=r$y) 
    pl <- pl + geom_line(data=r$f, aes(x=time, y=f), color=cl0, size=sz0)+
      geom_point(aes(x=time, y=y), color=cl1, size=sz1) 
    
    print(pl)
    
    
  })
  
  
  output$tabler <- renderTable({
    r()$result},
    digits = 3
  )
  
  output$tables <- renderTable({
    r()$sse},
    digits = 3
  )
  
})

errpred <- function(pp,y,X,lambda){
  y.pred= X%*%pp
  d <- length(pp)
  e=sum((y.pred-y)^2) + lambda*sum(pp[2:d]^2)
  return(e)
}