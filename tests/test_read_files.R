## Basic test using 2 in-built files

library('BoluspH')

testdir <- system.file("extdata", package="BoluspH")

meta <- data.frame(ID=c('Cow_1','Cow_2'), Filename=c('ST1.csv','ST2.csv'), Milking=c('08, 14:30'), stringsAsFactors=FALSE)

model <- BoluspH(meta)

model$AddCustomCSVData(testdir, skip=13, date_col=3, time_col=4, pH_col=6, sep=';', dec=',', date_format='%d.%m.%Y', time_format='%H:%M')
