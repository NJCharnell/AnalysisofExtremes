data(venice)
attach(venice)
head(venice)

#Figure 4.1
pdf(file="maxvenice.pdf", width=7, height=5)
plot(Year,r1,xaxt="n", yaxt="n", xlab="", ylab="")
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)
title(xlab="Year", ylab="Sea level", cex.lab=1.2)
dev.off()

#Fitting the GEV model and finding the covariance matrix
slfit <- gev.fit(r1)
slfit$cov

#Confidence Intervals
slfit$mle[1] +c(-1,1)*qnorm(0.975)*sqrt(slfit$cov[1,1])   
slfit$mle[2] +c(-1,1)*qnorm(0.975)*sqrt(slfit$cov[2,2])
slfit$mle[3] +c(-1,1)*qnorm(0.975)*sqrt(slfit$cov[3,3])

#10 year return level p=0.1
p=0.1
muhat =  slfit$mle[1]
sigmahat = slfit$mle[2]
epshat = slfit$mle[3]
y=-1/(log(1-p))
xphat = muhat - (sigmahat/epshat)*(1-(y^epshat)) 

#Profile likelihood for 10 year return period Figure 4.3
gev.prof(slfit, 10, 130, 170, conf = 0.95, nint = 100)

#Infinite observation return period, upper bound and standard error
x0hat = slfit$mle[1]- (slfit$mle[2]/slfit$mle[3])
x0hat

gradx01=1
gradx02=-1/epshat
gradx03=sigmahat/(epshat)^2
gradx0=c( gradxp1,gradxp2,gradxp3)
gradx0

varx0 = gradx0%*%slfit$cov%*%gradx0
varx0
sqrt(varx0)

#Profile likelihood for epsilon   Figure 4.2
M1 <- fgev(r1, method="Nelder-Mead")
M1
M1P <- profile(M1, which="shape")
M1P
plot(M1P, ci = c(0.95),main="",cex.axis=1.2, cex.lab=1.2)
abline(v=-0.1975)
abline(v=0.098)


#Diagnostic plot for GEV fit Figure 4.4
gev.diag(slfit)


#Gumbel fit and diagnostic plot Figure 4.5

slgfit <- gum.fit(r1)
slgfit$cov

gum.diag(slgfit)