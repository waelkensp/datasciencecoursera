# Calculs pour proportion

path <- file.path(getwd(),"OCR","guerison.txt")
guerison <- read.table(path, header = T)

n_total <- dim(guerison)[1]
n_gueris <- sum(guerison$guerison)
p_estim <- n_gueris/n_total
round(p_estim, 3)

alpha <- 0.05
icinf <- p_estim - qnorm(p=1-alpha/2) * sqrt(p_estim*(1-p_estim)/n_total)
round(icinf, 3)

icisup <- p_estim + qnorm(p=1-alpha/2) * sqrt(p_estim*(1-p_estim)/n_total)
round(icisup, 3)

prop.test(n_gueris, n_total, conf.level = 1-alpha)
binom.test(n_gueris, n_total, conf.level = 1-alpha)


# Calculs sur moyenne

path <- file.path(getwd(),"OCR","essence.txt")
essence <- read.table(path, header = T)
n_total <- dim(essence)[1]
xbar <- mean(essence$conso)
sprime <- sd(essence$conso)

alpha <- 0.05
icinf <- xbar - qt(p=1-alpha/2, df=n_total-1) * sprime/sqrt(n_total)
round(icinf, 2)

icsup <- xbar + qt(p=1-alpha/2, df=n_total-1) * sprime/sqrt(n_total)
round(icsup, 2)

t.test(essence$conso, conf.level = 1-alpha)


# Calculs sur variance

sprime2 <- var(essence$conso)

alpha <- 0.05
icinf <- (n_total-1)*sprime2/qchisq(p=1-alpha/2, df=n_total-1)
round(icinf, 2)

icsup <- (n_total-1)*sprime2/qchisq(p=alpha/2, df=n_total-1)
round(icsup, 2)

library(EnvStats)
varTest(essence$conso, conf.level = 1-alpha)
