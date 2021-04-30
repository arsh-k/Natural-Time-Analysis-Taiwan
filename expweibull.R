pexpweibull<- function(t,lambda,kappa,alpha,log.p=FALSE){
  log.cdf <- alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE)
  ifelse(log.p, return(log.cdf), return(exp(log.cdf)))
}  

# Probability density function
dexpweibull<- function(t,lambda,kappa,alpha,log=FALSE){
  log.pdf <-  log(alpha) + (alpha-1)*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) + 
    dweibull(t,scale=lambda,shape=kappa,log=TRUE)
  ifelse(log, return(log.pdf), return(exp(log.pdf)))
}

# Quantile function
qexpweibull<- function(p,lambda,kappa,alpha,log.p=FALSE){
  quant <-  qweibull(p^(1/alpha),scale=lambda,shape=kappa,log.p = TRUE)
  ifelse(log, return(quant), return(exp(quant)))
}  

# Random number generation
rexpweibull<- function(n,lambda,kappa,alpha,log=FALSE){
  u = runif(n)
  sim <-  qweibull(u^(1/alpha),scale=lambda,shape=kappa)
  return(sim)
} 

# Hazard function
hexpweibull <- function(t,lambda,kappa,alpha,log=FALSE){
  log.pdf <-  log(alpha) + (alpha-1)*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) + 
    dweibull(t,scale=lambda,shape=kappa,log=TRUE)
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  log.h <- log.pdf - log(1-cdf)
  ifelse(log, return(log.h), return(exp(log.h)))
}                                                                                      

# Cumulative hazard function
CHexpweibull <- function(t,lambda,kappa,alpha,log.p=FALSE){
  cdf <- exp(alpha*pweibull(t,scale=lambda,shape=kappa,log.p=TRUE) )
  return(-log(1-cdf))
}