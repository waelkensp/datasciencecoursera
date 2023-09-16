path <- file.path(getwd(),"Data","getdata_data_ss06hid.csv")
commu <- read.csv(path)
head(commu)

head(commu[which(commu$AGS == 6 & commu$ACR == 3),1:3], 3)


path2 <- file.path(getwd(),"Data","getdata_jeff.jpg")
pict <- readJPEG(path2, native = TRUE)
quantile(pict, probs=c(0.3, 0.8))


path3 <- file.path(getwd(),"Data","getdata_data_EDSTATS_Country.csv")
country <- read.csv(path3)
path4 <- file.path(getwd(),"Data","getdata_data_GDP.csv")
gdp <- read.csv(path4)

country2 <- select(country, CountryCode:Income.Group)
country2
match(country2$CountryCode, gdp$Name)
dim(country2)
dim(gdp)
reuni <- merge(country2, gdp, by.x="CountryCode", by.y="Name")
reuni2 <- reuni[(!is.na(reuni$Ranking)),]
reuni2 <- arrange(reuni2, desc(Ranking))
reuni2

gdp$
head(country)
names(country)
head(gdp)
names(gdp)
match(country$CountryCode, gdp$Name)
gdp2 <- arrange(gdp, desc(Ranking))
tail(gdp2)     
gdp2
reuni <- merge(country, gdp, by.x="CountryCode", by.y="Name")
names(reuni)
gdp2 <- arrange(reuni, desc(Ranking))

head(gdp2, 14)
head(gdp)
