#Question2b
load("~/Desktop/dataex2.rdata")
library(miscTools)
library(maxLik)
#log likelihood functuon
log_like <- function(param, data){
  x <- data[,1]; r <- data[,2]
  mu=param
  sum(r*dnorm(x,mean=mu,sd=1.5,log=TRUE)+(1-r)*pnorm(x,mean=mu,sd=1.5,log.p = TRUE))
}
#MLE
mle <- maxLik(logLik = log_like, data = dataex2, start = c(mu=1))
summary(mle)

#Question4
load("~/Desktop/dataex4.rdata")
#remove missing data
dataex4$Y<- replace(dataex4$Y, is.na(dataex4$Y), 0)
x <- dataex4$X
y <- dataex4$Y
#Q
Q<- function(param,data){
  beta0 <- param[1];
  beta1 <- param[2]
  sum(y* (beta0 + beta1*x) - log(1 + exp(beta0 + beta1*x)))
}
b1 <-c(0,0)
repeat{
  b2 <- coef(maxLik(Q, data = dataex4)，start = c(0,0))
  if (abs(b2[1] - b1[1]) + abs(b2[2] - b1[2]) < 0.001){
    break
  }
  b1 <- b2
}
b2


#Question5b
load("~/Desktop/dataex5.rdata")
y<-dataex5
em.mixture.two.poisson <- function(y, theta0, eps){
  n <- length(y)
  theta <- theta0
  p <- theta[1]; lambda <- theta[2]; mu<- theta[3]
  diff <- 1
  while(diff > eps){
    theta.old <- theta
    #E-step
    ptilde1 <- p*(lambda*y^(-lambda-1))
    ptilde2 <- (1-p)*(mu*y^(-mu-1))
    ptilde <- ptilde1/(ptilde1 + ptilde2)
    #M-step
    p <- mean(ptilde)
    lambda <- sum(ptilde)/sum(ptilde*log(y))
    mu <- sum((1-ptilde))/sum((1-ptilde)*log(y))
    theta <- c(p, lambda, mu)
    diff <- sum(abs(theta-theta.old))
  }
  return(theta)
}
res <- em.mixture.two.poisson(y = y, theta0 = c(0.3,0.3,0.4), eps = 0.0001)
pest <- res[1]; lambdaest <- res[2]; muest <- res[3]
#get the estimate of p,lambda and mu
pest; lambdaest; muest
#draw the histpgram
p<-0.7939337
lambda<-0.9762783
mu<- 6.670599
y<-dataex5
hist(y,breaks = "Freedman-Diaconis", main = "Estimated density supermposed", 
     xlab = "y",
     ylab = "Density",
     cex.main = 1.5, cex.lab = 1.5, cex.axis = 1.4, freq = F,xlim=c(0,25), ylim = c(0,0.5))
curve((p*(lambda*x^(-lambda-1))+(1-p)*(mu*x^(-mu-1))), add = TRUE, lwd = 2, col = "blue")

