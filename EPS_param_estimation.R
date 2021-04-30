library(readxl)
library(fitdistrplus)
library(actuar)
source("expweibull.R")

data <- read.csv('interevent_counts.csv')   #to convert data type to vector

data2 = unname(unlist(data))

A<-fitdist(data2, 'weibull')
B<-fitdist(data2, 'lnorm')
C<-fitdist(data2, 'gamma',method = "mle")
D<-fitdist(data2, 'exp',method = "mle")
C1<-fitdist(data2, 'gamma',method = "mme")
D1<-fitdist(data2, 'exp',method = "mme")
E<-fitdist(data2, 'invgauss',start = list(mean = 5, shape = 1))
X<-fitdist(data2,'invweibull')
Y<-fitdist(data2, 'norm')
Z<-fitdist(data2, 'expweibull', start = list(kappa = 1, lambda = 1, alpha=1))
# histogram of the data
hist(data2)
# Summary of the results
summary(A)
summary(B)
summary(C)
summary(D)
summary(C1)
summary(D1)
summary(E)
summary(X)
summary(Y)
summary(Z)

#plot the data
a<-c("Weibull","lognormal","gamma", "exponential" , 'expweibull')

#PDF and CDF plots
denscomp(list(A,B,C,D,Z),legendtext = a,fitlty = 'solid', xlab = 'Interevent Counts')
cdfcomp(list(A,B,C,D,Z),legendtext=a, fitlty = 'solid', xlab = 'Interevent Counts', horizontals=TRUE, main = 'Figure 5: Best fit distribution CDFs and ECDF curve for dataset')
cdfcomp(list(Z),legendtext='expweibull', fitlty = 'solid', xlab = 'Interevent Counts', horizontals=TRUE, main = 'Figure 6: Exponentiated Weibull Distribution used to obtain EPS Score')

#Perfoming KS Test, the parameters can be added depending on MOM or MLE estimates
ks.test(data2,"pweibull",shape =1.06983 , scale = 74.80112)
ks.test(data2,"plnorm", meanlog = 3.793244, sdlog = 1.124995)
ks.test(data2,"pgamma",shape = 1.14987772, rate = 0.01579841 )
ks.test(data2,"pexp", rate = 0.01373932)
ks.test(data2,"pinvgauss",mean = 72.71221, shape = 23.01678)
ks.test(data2,"pinvweibull",shape = 0.7205068 , scale = 24.2254196)
ks.test(data2, "pnorm",mean = 72.78378,sd = 70.74550)
ks.test(data2, "pexpweibull",  kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)

# Perform Goodness of Fit MLE tests
gfMLE<-gofstat(list(A, B, C, D, E, X, Y, Z))
print(gfMLE)

#Evaluating EPS scores according to the best fit distribution for various radii
eps_tainan_R100 <- pexpweibull(45, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_tainan_R100)

eps_tainan_R150 <- pexpweibull(119, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_tainan_R150)

eps_tainan_R200 <- pexpweibull(61, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_tainan_R200)

eps_taipei_R100 <- pexpweibull(215, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_taipei_R100)

eps_taipei_R150<- pexpweibull(41, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_taipei_R150)

eps_taipei_R200<- pexpweibull(48, kappa = 0.8186113, lambda = 46.3922652, alpha=1.6679260)
print(eps_taipei_R200)
