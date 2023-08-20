path <- file.path(getwd(),"OCR","essence.txt")
essence <- read.table(path, header = T)

xbar <- mean(essence$conso)
round(xbar, 2)

sprime <- sd(essence$conso)
round(sprime,2)

sprime2 <- var(essence$conso)
round(sprime2, 2)

hist(essence$conso, prob=T, xlab="", ylab="", ylim=c(0,0.25), main='Histogramme')
abline(v = xbar, col="blue", lty=1, lwd=3)
legend("topright",legend=("Moyenne empirique"),col="blue",lty=1,lwd=3)
mu0 <- 31
abline(v=mu0,col="red",lwd=3)
legend("topright",legend=c("Moyenne empirique","Seuil testé"),col=c("blue","red"),lty=1,lwd=3)
curve(dnorm(x,mean=xbar,sd=sprime),col="red",lwd=2,add=TRUE,yaxt="n")
4+3
