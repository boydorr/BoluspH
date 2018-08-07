## Basic test using 2 in-built files

library('BoluspH')

testdir <- system.file("extdata", package="BoluspH")


# Giving start/end:
meta <- data.frame(ID=c('Cow_1','Cow_2','Cow_3','Cow_4','Cow_5'), Filename=c('CS1.csv','CS2.csv','ST1.xlsx','WC1.csv','WC2.csv'), Start=as.Date('1900-01-01'), End=as.Date('3000-01-01'), Farm=c('A','A','B','C','C'), stringsAsFactors=FALSE)
model <- BoluspH(meta)
model$AddCustomData(testdir, ID=c('Cow_1','Cow_2'), skip=13, date_col=3, time_col=4, pH_col=6, csv=TRUE, sep=';', dec=',', date_format='%d.%m.%Y', time_format='%H:%M')
model$AddWellCowData(testdir)
model$AddSmaXtecData(testdir)

# Giving milking:
meta <- data.frame(ID=c('Cow_1','Cow_2','Cow_3','Cow_4','Cow_5'), Filename=c('CS1.csv','CS2.csv','ST1.xlsx','WC1.csv','WC2.csv'), Milking=c('08, 14:30'), Farm=c('A','A','B','C','C'), stringsAsFactors=FALSE)
model <- BoluspH(meta)
model$AddCustomData(testdir, ID=c('Cow_1','Cow_2'), skip=13, date_col=3, time_col=4, pH_col=6, csv=TRUE, sep=';', dec=',', date_format='%d.%m.%Y', time_format='%H:%M')
model$AddWellCowData(testdir)
model$AddSmaXtecData(testdir)



dat <- model$ExtractData()

model$RunModels(gam=FALSE)

head(model$Daily)
head(model$Hourly)

dat <- model$ExtractData()
head(dat)

library('tidyverse')
ggplot(dat, aes(x=Time, y=prediction)) +
	geom_line() +
	facet_wrap(~ID, scale='free')
