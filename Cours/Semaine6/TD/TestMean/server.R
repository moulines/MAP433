#-------------------------------------------------------------------------
#  This application is governed by the CeCILL-B license. 
#  You can  use, modify and/ or redistribute this code under the terms
#  of the CeCILL license:  http://www.cecill.info/index.en.html
#
#  Marc Lavielle, Inria Saclay
#  July 8th, 2015
#-------------------------------------------------------------------------

library(gridExtra)
library(ggplot2)

# froot1 <- function(n,sigma,mu,eta,alpha){
#   sn <- sqrt(n)/sigma
#   S <- qnorm(1-alpha/2)/sn
#   f <- 1 - pnorm(sn*(S-mu)) + pnorm(sn*(-S-mu))
#   return(f)
# }

ggplotmlx <- function(...) {ggplot2::ggplot(...) + theme_bw() +
                              theme(plot.background = element_rect(fill=rgb(1,1,1))) }

shinyServer(function(input, output, session) {
  
  
  #   s1 <- qnorm(0.0001)
  #   S  <- seq(s1, -s1, length.out=100)
  
  observe({
    if (input$test == "1")
      updateSliderInput(session, "mu", min = 0 )
    else
      updateSliderInput(session, "mu", min = -5 )
  })
  
  
  r <- reactive({
    msg.test2 <- "Not available for two-sided test"
    
    if (input$xaxis=="1"){
      n <- seq(1,100,by=1)
      x <- n
      xlab <- "sample size (n)"
    }else
      n <- input$n
    
    if (input$xaxis=="2"){
      sigma <- seq(0.1,10,by=0.1)
      x <- sigma
      xlab <- "standard deviation (sigma)"
    }else
      sigma <- input$sigma
    
    if (input$xaxis=="3"){
      if (input$test=="1")
        mu <- seq(-1,5,by=0.1)
      else
        mu <- seq(-5,5,by=0.1)
      x <- mu
      xlab <- "mean (mu)"
    }else
      mu <- input$mu
    
    if (input$xaxis=="4"){
      level <- seq(0.01,0.99,by=0.01)
      x <- level
      xlab <- "level (alpha)"
    }else
      level <- input$level
    
    if (input$xaxis=="5"){
      power <- seq(0.01,0.99,by=0.01)
      x <- power
      xlab <- "power (1-beta)"
    }else
      power <- input$power
    
    if (input$xaxis=="6"){
      if (input$test=="1")
      s <- seq(-4,4,by=0.1)
      else
        s <- seq(0,4,by=0.1)
      x <- s
      xlab <- "critical value"
    }    
    x1 <- x
    x2 <- x
    df <- n-1
    
    if (input$yaxis=="1"){
      if (input$xaxis=="1"){
        y1 <- x1
        y2 <- x2
      } else if (input$xaxis=="6"){
        if (input$test == "1"){
          r1 <- qnorm(1-level)/x*sigma
          r2 <- qt(1-level,df)/x*sigma
        }else{
          r1 <- qnorm(1-level/2)/x*sigma
          r2 <- qt(1-level/2,df)/x*sigma
        }
        ir <- which(r1>0 & r1<Inf)
        x1 <- x[ir]
        y1 <- r1[ir]^2
        ir <- which(r2>0 & r2<Inf)
        x2 <- x[ir]
        y2 <- r2[ir]^2
      }else{
        if (input$test == "2")
          stop(msg.test2)      
        r1 <- (qnorm(1-level)-qnorm(1-power))*sigma/mu
        r2 <- (qt(1-level,df)-qt(1-power,df))*sigma/mu
        ir <- which(r1>0 & r1<Inf)
        x1 <- x[ir]
        y1 <- r1[ir]^2
        ir <- which(r2>0 & r2<Inf)
        x2 <- x[ir]
        y2 <- r2[ir]^2
      }
      y1 <- pmax(1,y1)
      y2 <- pmax(1,y2)
      ylab <- "sample size (n)"
      
    }else if (input$yaxis=="2"){
      if (input$xaxis=="2"){
        y1 <- x1
        y2 <- x2
      } else if (input$xaxis=="6"){
        if (input$test == "1"){
          y1 <- x/qnorm(1-level)*sqrt(n)
          y2 <- x/qt(1-level,df)*sqrt(n)
        }else{
          y1 <- x/qnorm(1-level/2)*sqrt(n)
          y2 <- x/qt(1-level/2,df)*sqrt(n)
        }
        x1 <- x
        x2 <- x
      }else{
        if (input$test == "2")
          stop(msg.test2)
        r1 <- sqrt(n)*mu/(qnorm(1-level)-qnorm(1-power))
        r2 <- sqrt(n)*mu/(qt(1-level,df)-qt(1-power,df))
        ir <- which(r1>0 & r1<Inf)
        x1 <- x[ir]
        y1 <- r1[ir]
        ir <- which(r2>0 & r2<Inf)
        x2 <- x[ir]
        y2 <- r2[ir]
      }
      ylab <- "standard deviation (sigma)"
      
    }else if (input$yaxis=="3"){
      if (input$xaxis=="3"){
        y1 <- x1
        y2 <- x2
      }else{
        if (input$test == "2")
          stop(msg.test2)
        y1 <- sigma/sqrt(n)*(qnorm(1-level)-qnorm(1-power))
        y2 <- sigma/sqrt(n)*(qt(1-level,df)-qt(1-power,df))
      }
      ylab <- "mean (mu)"
      
    }else if (input$yaxis=="4"){
      if (input$xaxis=="4"){
        y1 <- x1
        y2 <- x2
      }else{
        if (input$test == "1"){
          if (input$xaxis=="6"){
            y1 <- 1 - pnorm(s*sqrt(n)/sigma)
            y2 <- 1 - pt(s*sqrt(n)/sigma,df)
          }else{
            y1 <- 1 - pnorm(sqrt(n)*mu/sigma + qnorm(1-power))
            y2 <- 1 - pt(sqrt(n)*mu/sigma + qt(1-power,df),df)            
          }
        }else{
          if (input$xaxis=="6"){
            y1 <- 2*(1 - pnorm(s*sqrt(n)/sigma))
            y2 <- 2*(1 - pt(s*sqrt(n)/sigma,df))
          }else{
            stop(msg.test2)
          }
        }
      }
      ylab <- "level (alpha)"
      
    }else if (input$yaxis=="5"){
      if (input$xaxis=="5"){
        y1 <- x1
        y2 <- x2
      }else{
        if (input$test == "1"){
          if (input$xaxis=="6"){
            y1 <- 1 - pnorm((s-mu)*sqrt(n)/sigma)
            y2 <- 1 - pt((s-mu)*sqrt(n)/sigma,df)
          }else{
            y1 <- 1 - pnorm(qnorm(1-level) - sqrt(n)*mu/sigma)
            y2 <- 1 - pt(qt(1-level,df) - sqrt(n)*mu/sigma,df)
          }
        }else{
          if (input$xaxis=="6"){
            y1 <- 1 - pnorm(s - sqrt(n)*mu/sigma) + pnorm(-s - sqrt(n)*mu/sigma)
            y2 <- 1 - pt(s - sqrt(n)*mu/sigma,df) + pt(-s - sqrt(n)*mu/sigma,df)
          }else{
            S1 <- qnorm(1-level/2)
            y1 <- 1 - pnorm(S1 - sqrt(n)*mu/sigma) + pnorm(-S1 - sqrt(n)*mu/sigma)
            S2 <- qt(1-level/2,df)
            y2 <- 1 - pt(S2 - sqrt(n)*mu/sigma,df) + pt(-S2 - sqrt(n)*mu/sigma,df)
          }
        }
      }
      ylab <- "power (1-beta)"
    }else if (input$yaxis=="6"){
      if (input$xaxis=="6"){
        y1 <- x1
        y2 <- x2
      }else{
        if (input$test == "1"){
          y1 <- qnorm(1-level)*sigma/sqrt(n) 
          y2 <- qt(1-level, df)*sigma/sqrt(n) 
        }else{
          y1 <- qnorm(1-level/2)*sigma/sqrt(n) 
          y2 <- qt(1-level/2, df)*sigma/sqrt(n) 
        }
      }
      ylab <- "critical value (c)"
    }
    r <- list(d1=data.frame(x1,y1),d2=data.frame(x2,y2),xlab=xlab,ylab=ylab)
  })
  
  output$plot <- renderPlot({
    r <- r()
    pl <-ggplotmlx() 
    if (any(input$sigmaest=="1"))
      pl <- pl + geom_line(data=r$d1, aes(x1,y1),size=1, colour="blue") 
    if (any(input$sigmaest=="2"))
      pl <- pl + geom_line(data=r$d2, aes(x2,y2),size=1, colour="red")  
    pl <- pl + xlab(r$xlab) + ylab(r$ylab)    
    if (input$log=="2")
      pl <- pl + scale_y_log10()
    print(pl)
  })
  
})