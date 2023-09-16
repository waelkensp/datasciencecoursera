path <- file.path(getwd(),"Data","getdata_data_ss06hid.csv")
micro <- read.csv(path, header = T)
micro$VAL
val2 <- micro$VAL[!is.na(micro$VAL)]
length(val2[val2==24])


library(xlsx)
path <- file.path(getwd(),"Data","getdata_data_DATA.gov_NGAP.xlsx")
gas <- read.table(path, header = T)

library(XML)
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml"
doc <- xmlTreeParse(fileUrl, useInternal = TRUE)
fileUrl


library (RCurl)
library (XML)
curlVersion()$features
curlVersion()$protocol
##These should show ssl and https. I can see these on windows 8.1 at least. 
##It may differ on other OSes.

temp <- getURL("https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Frestaurants.xml", ssl.verifyPeer=FALSE)
DFX <- xmlTreeParse(temp,useInternal = TRUE)

rootNode <- xmlRoot(DFX)
names(rootNode)
rootNode
xmlSApply(rootNode, xmlValue)
test <- xpathSApply(rootNode, "//zipcode", xmlValue)
test[test == "21231"]



library(data.table)
path <- file.path(getwd(),"Data","getdata_data_ss06pid.csv")
DT<- fread(path)

start_time <- Sys.time()
mean(DT[DT$SEX==1,]$pwgtp15); mean(DT[DT$SEX==2,]$pwgtp15)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
mean(DT$pwgtp15,by=DT$SEX)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
tapply(DT$pwgtp15,DT$SEX,mean)
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
DT[,mean(pwgtp15),by=SEX]
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
rowMeans(DT)[DT$SEX==1]; rowMeans(DT)[DT$SEX==2]
end_time <- Sys.time()
end_time - start_time

start_time <- Sys.time()
sapply(split(DT$pwgtp15,DT$SEX),mean)
end_time <- Sys.time()
end_time - start_time