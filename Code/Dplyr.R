library(dplyr)

path <- file.path(getwd(),"Data","chicago.rds")
chicago <- readRDS(path)

str(chicago)
names(chicago)
head(select(chicago, city:dptp))
head(select(chicago, -(city:dptp)))

chic.f <- filter(chicago, pm25tmean2 > 30)
head(chic.f)

chicago <- arrange(chicago, desc(date))

chicago <- rename(chicago, pm25 = pm25tmean2)

chicago <- mutate(chicago, pm25detrend = pm25-mean(pm25, na.rm=TRUE))
head(chicago)
head(select(chicago, pm25, pm25detrend))

chicago <- mutate(chicago, tempcat = factor(1 * (tmpd>80), labels=c("cold", "hot")))
hotcold <- group_by(chicago, tempcat)
hotcold
summarize(hotcold, pm25=mean(pm25, na.rm=TRUE), o3 = max(o3tmean2), no2 = median(no2tmean2))

chicago %>% mutate(month)