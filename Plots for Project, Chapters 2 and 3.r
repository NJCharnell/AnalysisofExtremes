#Chapter 2

#Fig 2.1: Distribution functions for 3 types of GEV
curve(pgumbel, xlim=c(-10,10), col="black", xaxt="n", yaxt="n", xlab="", ylab="")
curve(pfrechet, add=TRUE, col="red")
curve(pnweibull, add=TRUE, col="blue")
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)
title(xlab="x-value", ylab="Distribution Function", cex.lab=1.1)

colors <- c("black", "red", "blue")
 labels <- c(expression("Gumbel; a=1, b=0"), expression("Frechet; a=1, b=0,"~alpha~"=1"),expression("Weibull; a=1, b=0, "~alpha~"=1"))
legend("bottomright", inset=.01,
   labels, lwd=2, lty=c(1, 1, 1, 1, 1), cex=1,col=colors)

#Figure 2.2: Return Level plot
p=seq(0,1,length.out=1000)
y = -1/log(1-p)
xgum = log(y)
plot(log(y), xgum, type='l', xaxt="n", yaxt="n", xlab="", ylab="")
xfre = -(1/0.2)*(1-y^(0.2))
lines(log(y),xfre, type='l', col="red")
xwei = -(1/-0.2)*(1-y^(-0.2))
lines(log(y),xwei, type='l', col="blue")
axis(1,cex.axis=1.2)
axis(2,cex.axis=1.2)
title(xlab=expression(log(y[p])), ylab="Return Level", cex.lab=1.2)

legend("bottomright",inset=.05,c(expression(epsilon==0),expression(epsilon==0.2),expression(epsilon==-0.2)), cex=1.2, fill=c("black","red","blue"))

##############################################################################################################################################################

#Chapter 3

#Fig 3.1: Gumbel pdf
curve(dgumbel, xlim=c(-10,10), xlab="",ylab="", xaxt="n", yaxt="n") #plots Gumbel distribution
axis(1,cex.axis=1.4)
axis(2,cex.axis=1.4)
title(xlab="x", ylab="PDF", cex.lab=1.4)

#Fig 3.2: Frechet pdf
curve(dfrechet, xlim=c(-10,10),ylim=c(0,0.6), xlab="",ylab="", xaxt="n", yaxt="n")
axis(1,cex.axis=1.4)
axis(2,cex.axis=1.4)
title(xlab="x", ylab="PDF", cex.lab=1.4)

#Fig 3.3: Weibull pdf with alpha=1
curve(drweibull, xlim=c(-10,10),ylim=c(0,1.1), xlab="",ylab="", xaxt="n", yaxt="n")
axis(1,cex.axis=1.4)
axis(2,cex.axis=1.4)
title(xlab="x", ylab="PDF", cex.lab=1.4)

#Fig 3.4: Weibull pdf with alpha=1 arond x=0
curve(drweibull, xlim=c(-1,1),ylim=c(0,1.1), xlab="",ylab="", xaxt="n", yaxt="n")
axis(1,cex.axis=1.4)
axis(2,cex.axis=1.4)
title(xlab="x", ylab="PDF", cex.lab=1.4)

#Fig 3.5: Weibull pdf with alpha=0.5
x = seq(-10,10, length.out=1000)
drweibull2 <- drweibull(x, loc=0, scale=1, shape=0.5,log=FALSE)
plot(x,drweibull2, type='l', xlab="",,ylab="", xaxt="n", yaxt="n")
axis(1,cex.axis=1.4)
axis(2,cex.axis=1.4)
title(xlab="x", ylab="PDF", cex.lab=1.4)
