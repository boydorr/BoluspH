## Basic test using in-built files

library('BoluspH')

testdir <- system.file("extdata", package="BoluspH")

# Giving nothing:
meta <- data.frame(ID=c('Cow_1','Cow_2','Cow_3','Cow_4','Cow_5'), Filename=c('CS1.csv','CS2.csv','ST1.xlsx','WC1.csv','WC2.csv'), stringsAsFactors=FALSE)
model <- BoluspH(meta)
model$AddCustomData(testdir, ID=c('Cow_1','Cow_2'), skip=13, date_col=3, time_col=4, pH_col=6, csv=TRUE, sep=';', dec=',', date_format='%d.%m.%Y', time_format='%H:%M')
model$AddWellCowData(testdir)
model$AddSmaXtecData(testdir)
model$RunModels(gam=TRUE)
data_out <- model$ExtractData()
overview_out <- model$GetOverview()

overview_out

library('tidyverse')
ggplot(data_out, aes(x=Time)) +
	geom_line(mapping=aes(y=pH)) +
	geom_line(mapping=aes(y=prediction), col='blue', lwd=2) +
	facet_wrap(~ID, scale='free')

head(data_out)

daily <- data_out %>% group_by(ID, Date) %>%
	summarise(mean_abs_resid = mean(abs(residual)))
head(daily)

ggplot(daily, aes(x=Date, y=mean_abs_resid, col=ID)) +
	geom_line() +
	facet_wrap(~ID, scales='free_x')

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

# Giving other stuff just 2 files:
meta <- data.frame(ID=c('Cow_1','Cow_2'), Filename=c('ST1.xlsx','WC1.csv'), Milking=c('09:00,16:00', '07:00,13:00,18:00'), Farm=c('FarmA','FarmB'), stringsAsFactors=FALSE)
model <- BoluspH(meta)
model$AddWellCowData(testdir)
model$AddSmaXtecData(testdir)


dat <- model$ExtractData()

model$RunModels(gam=TRUE)

head(model$Daily)
head(model$Hourly)

dat <- model$ExtractData()
head(dat)

library('tidyverse')
ggplot(dat, aes(x=Time)) +
	geom_point(mapping=aes(y=pH)) +
	geom_line(mapping=aes(y=prediction), col='blue', lwd=2) +
	facet_wrap(~ID, scale='free')

ggplot(model$Daily, aes(x=Date, y=GAMpred)) +
	geom_line() +
	facet_wrap(~ID, scale='free')
