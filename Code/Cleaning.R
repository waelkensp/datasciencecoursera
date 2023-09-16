install.packages("reshape2")

library(reshape2)
head(mtcars)

mtcars$carname <- rownames(mtcars)
head(mtcars)
carMelt <- melt(mtcars, id=c("carname", "gear", "cyl"), measure.vars=c("mpg", "hp"))
carMelt

cylData <- dcast(carMelt, cyl ~ variable)
cylData
cylData <- dcast(carMelt, cyl ~ variable, mean)
cylData

head(InsectSprays)
tapply(InsectSprays$count, InsectSprays$spray, sum)

spIns = split(InsectSprays$count, InsectSprays$spray)
spIns
sprCount = lapply(spIns, sum)
sprCount

unlist(sprCount)
sapply(spIns, sum)

library(plyr)
ddply(InsectSprays,.(spray), summarize, sum=sum(count))
spraySums <- ddply(InsectSprays,.(spray), summarize, sum=ave(count, FUN=sum))
head(spraySums)
