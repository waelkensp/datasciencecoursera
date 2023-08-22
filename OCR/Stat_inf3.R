path <- file.path(getwd(),"OCR","guerison.txt")
guerison <- read.table(path, header = T)
n_total <- dim(guerison)[1]
n_gueris <- sum(guerison$guerison)

prop.test(x=n_gueris, n=n_total, p=0.75, alternative="greater")

prop.test(x=399284, n=781170, p=0.5, alternative="greater")



path <- file.path(getwd(),"OCR","essence.txt")
essence <- read.table(path, header = T)

mu0 <- 31
t.test(essence$conso, mu=mu0, alternative="two.sided")

library(EnvStats)
varTest(essence$conso, sigma.squared = 4.5, alternative = "greater")

versi <- iris[iris$Species=="versicolor",]$Petal.Length
virgi <- iris[iris$Species=="virginica",]$Petal.Length
var.test(versi, virgi)
t.test(versi, virgi, var.equal = T)

# 26306 fois 12 dés => Nombre de 5 ou 6 obtenus
n <- c(185,1149,3265,5475,6114,5194,3067,1331,403,105,14,4,0)
p0 <- dbinom(0:12,12,1/3)
chisq.test(n, p=p0)

n <- c(185,1149,3265,5475,6114,5194,3067,1331,403,105,18)
p0 <- c(dbinom(0:9,12,1/3), sum(dbinom(10:12,12,1/3)))
chisq.test(n, p=p0)

#loi continue
path <- file.path(getwd(),"OCR","essence.txt")
essence <- read.table(path, header = T)

ks.test(essence$conso, "pnorm", mean=mean(essence$conso), sd=sd(essence$conso))
shapiro.test(essence$conso)


prop.test(x=399284, n=781167, p=0.5, alternative="two.sided")
