#' @title Import and analysis of rumen pH bolus data
#' @name BoluspH
#' @aliases BoluspH bolusph

#' @description
#' This function is used to import one or more rumen pH bolus data files and store the data for later analysis.  The return value is a data storage class containing all methods required to access, analyse and summarise the data.

#' @details
#' The data storage class is written using an OOP-style of programming, so that the data can be accessed in different formats at any point using a single R object as the point of reference.  This minimises the possibilities for programming mistakes by the end user, but does result in code that may look a little strange to users who may be more used to the standard procedural style of R programming.  The best way to understand the code is using an example - see vignette('BoluspH', package='BoluspH') for an overview.

#' @return
#' An object of class BoluspH.

#' @examples
#' \dontrun{
#' vignette('BoluspH', package='BoluspH')
#' }

#' @param meta_data an optional data frame including the columns ID (giving a unique identifier for the cow) and filename (giving the filename used for that cow)


BoluspH <- setRefClass('BoluspH',
	fields = list(Meta='data.frame', Data='data.frame', Overview='data.frame', Daily='data.frame', Hourly='data.frame',id_imported='character'), 

	methods = list(

	initialize = function(meta_data=data.frame(ID=character(0), Filename=character(0))){
		"Set up a BoluspH object (with meta data) before importing files"
		
		if(!is.data.frame(meta_data) || !all(c('ID','Filename') %in% names(meta_data)) || nrow(meta_data) < 1){
			stop('The argument to meta_data must be a data frame including the columns ID (giving a unique identifier to be used for the cow), and Filename (giving the filename used for the corresponding cow). Optional columns are Milking (giving a character string with comma separated times of day, or a blank character string), Start (the first date to be used for analysis), and End (the last date to be used for analysis).  Other columns may also be used if required for later analysis.')
		}
		
		if(length(unique(meta_data$ID))!=nrow(meta_data))
			stop('Non-unique ID detected in the meta data')

		if(length(unique(meta_data$Filename))!=nrow(meta_data))
			stop('Non-unique Filename detected in the meta data')
		
		# Add Start and End dates if not included:
		if(!'Start' %in% names(meta_data)){
			meta_data$Start <- as.Date(NA)
		}
		if(!'End' %in% names(meta_data)){
			meta_data$End <- as.Date(NA)
		}
		if(!'Milking' %in% names(meta_data)){
			meta_data$Milking <- ''
		}
		stopifnot(inherits(meta_data$Start, 'Date'))
		stopifnot(inherits(meta_data$End, 'Date'))

		# Check/Interpret Milking times:
		tm <- meta_data$Milking
		tochange <- tm!=''
		meta_data$Milking <- interpret_times(meta_data$Milking, proportions=FALSE, warn=TRUE)
		
		.self$Meta <- meta_data %>% select(.data$ID, .data$Filename, .data$Milking, .data$Start, .data$End, everything())

		.self$Data <- data.frame(ID=factor(levels=.self$Meta$ID), Date=as.Date(character(0)), Time=as.POSIXct(character(0), tz='GMT'), pH=numeric(0))
		
	},
	
	AddCustomData = function(folder_path, ID='all', skip=1, date_col=1, time_col=2, pH_col=3, remove_start=0.5, remove_end=0.5, max_days=40, csv=TRUE, sep=',', dec='.', date_format='%Y-%m-%d', time_format='%H:%M'){
		"Import one or more rumen pH bolus data CSV/Excel files using a custom file format"
		
		if(!is.character(folder_path) || length(folder_path)!=1){
			stop('The argument to folder_path must be a single path to a folder containing ', ifelse(csv, 'CSV', 'Excel'), ' files matching the Filenames in the meta-data (to be imported)')
		}
		if(!is.numeric(skip) || length(skip)!=1){
			stop('The argument to skip must be a numeric value of length 1')
		}
		if(!is.numeric(date_col) || length(date_col)!=1){
			stop('The argument to date_col must be a numeric value of length 1')
		}
		if(!is.numeric(time_col) || length(time_col)!=1){
			stop('The argument to time_col must be a numeric value of length 1')
		}
		if(!is.numeric(pH_col) || length(pH_col)!=1){
			stop('The argument to pH_col must be a numeric value of length 1')
		}
		if('all' %in% ID)
			ID <- .self$Meta$ID[!.self$Meta$ID %in% .self$id_imported]
		if(any(ID %in% .self$id_imported))
			stop('Unable to re-import ID that have already been imported')
		
		files <- get_files(folder_path, extension=if(csv) 'csv' else c('xlsx','xls'), targets=.self$Meta$Filename[.self$Meta$ID %in% ID])
		cat('Importing ', nrow(files), ' files...\n')
		newdata <- vector('list', length=nrow(files))
		for(i in seq_len(nrow(files))){
			cat('\tReading: ', files$path[i], '\n')
			tID <- .self$Meta$ID[.self$Meta$Filename==files$Filename[i]]
			stopifnot(length(tID)==1)
			.self$id_imported <- c(.self$id_imported, tID)
			if(csv){
				newdata[[i]] <- read_csv_file(path=files$path[i], skip=skip, date_col=date_col, time_col=time_col, pH_col=pH_col, sep=sep, dec=dec, date_format=date_format, time_format=time_format, ID=tID)
			}else{
				newdata[[i]] <- read_excel_file(path=files$path[i], skip=skip, date_col=date_col, time_col=time_col, pH_col=pH_col, ID=tID)
			}
			
			# Remove start and end periods as necessary:
			sp <- min(newdata[[i]]$Time) + remove_start*60*60*24
			ep <- max(newdata[[i]]$Time) - remove_end*60*60*24
			ep <- min(ep, sp+max_days*60*60*24)
			newdata[[i]] <- as.data.frame( newdata[[i]] %>% filter(.data$Time >= sp, .data$Time <= ep) )
			
			# Adjust the meta data to reflect first and last whole days:
			sd <- min(newdata[[i]]$Date)+1
			sd <- max(sd, .self$Meta$Start[.self$Meta$ID == tID], na.rm=TRUE)
			ed <- max(newdata[[i]]$Date)-1
			ed <- min(ed, .self$Meta$End[.self$Meta$ID == tID], na.rm=TRUE)
			.self$Meta$Start[.self$Meta$ID == tID] <- sd
			.self$Meta$End[.self$Meta$ID == tID] <- ed
			
			# Give a warning if any date is missing more than 10% of obs:
			numobs <- newdata[[i]] %>% filter(.data$Date >= sd, .data$Date <= ed) %>% group_by(.data$Date) %>% summarise(Nobs=n()) %>% group_by(.data$Nobs) %>% tally
			if(any(numobs$Nobs < 0.9*max(numobs$Nobs))){
				warning(paste0('One or more date has >10% missing observations for ', tID))
			}
		}
		cat('\tDone\n')
		
		.self$Data <- rbind(.self$Data, do.call('rbind', newdata))
		
	},
	
	AddWellCowData = function(folder_path, ID='all', remove_start=0.5, remove_end=0.5, max_days=40, sep=',', dec='.'){
		"Import one or more rumen pH bolus data CSV files using the WellCow format"
		
		if(!is.character(folder_path) || length(folder_path)!=1){
			stop('The argument to folder_path must be a single path to a folder containing CSV files matching the Filenames in the meta-data (to be imported)')
		}
		
		# Standard WellCow format:
		skip <- 3
		date_col <- 2
		time_col <- 2
		pH_col <- 5
		date_format <- "%Y-%m-%dT%H:%M:%S%z"
		time_format <- "%Y-%m-%dT%H:%M:%S%z"		
		.self$AddCustomData(folder_path=folder_path, ID=ID, skip=skip, date_col=date_col, time_col=time_col, pH_col=pH_col, remove_start=remove_start, remove_end=remove_end, max_days=max_days, csv=TRUE, sep=sep, dec=dec, date_format=date_format, time_format=time_format)
		
	},

	AddSmaXtecData = function(folder_path, ID='all', remove_start=0.5, remove_end=0.5, max_days=40){
		"Import one or more rumen pH bolus data Excel files using the SmaXtec format"
		
		if(!is.character(folder_path) || length(folder_path)!=1){
			stop('The argument to folder_path must be a single path to a folder containing Excel files matching the Filenames in the meta-data (to be imported)')
		}
		
		# Standard SmaXtec format:
		skip <- 11
		date_col <- 1
		time_col <- 2
		pH_col <- 3
		.self$AddCustomData(folder_path=folder_path, ID=ID, skip=skip, date_col=date_col, time_col=time_col, pH_col=pH_col, remove_start=remove_start, remove_end=remove_end, max_days=max_days, csv=FALSE)
	},
	
	StartEndDate = function(ID, start_date, end_date){
		"Alter the first and last dates to be used for the analysis for a given individual"
		
		stopifnot(length(ID)==1 && (is.character(ID) || is.factor(ID)))
		if(is.factor(ID))
			ID <- as.character(ID)
		
		stopifnot(length(start_date)==1 && (is.numeric(start_date) || inherits(start_date, 'Date')))
		stopifnot(length(end_date)==1 && (is.numeric(end_date) || inherits(end_date, 'Date')))
		
		if(!ID %in% .self$ID_imported)
			stop('Data for this individual has not yet been imported')
		mt <- .self$Meta[.self$Meta$ID==ID,]
		
		if(is.numeric(start_date)){
			start_date <- mt$Start + start_date - 1
		}
		if(is.numeric(end_date)){
			end_date <- mt$Start + end_date
		}
		
		tdat <- .self$Data %>% filter(.data$ID == ID)
		sd <- min(tdat$Date)
		ed <- max(tdat$Date)
		
		if(start_date < sd){
			stop('Unable to set start date before the range of the data')
		}
		if(end_date < ed){
			stop('Unable to set end date before the range of the data')
		}
		
		.self$Meta$Start[.self$Meta$ID==ID] <- start_date
		.self$Meta$End[.self$Meta$ID==ID] <- end_date
		
		cat('Start and End dates updated for individual', ID, '\n')
	},
	
	ExtractData = function(ID = 'all'){
		"Extract the saved time series data for one more individual, including model predictions if the analysis has been run"
		
		stopifnot(is.character(ID))
		if('all' %in% ID){
			ID <- .self$id_imported
		}
		if(any(!ID %in% .self$id_imported)){
			stop('Data for one or more specified ID has not yet been imported')
		}
		
		return(.self$Data %>% filter(.data$ID %in% ID))
		
	},
	
	RunModels = function(gam=TRUE, milking='adjusted', exclude_pre=1, exclude_post=1){
		"Run the model using specified options and record the predictions and summary statistics for later use"
		
		if(any(!.self$Meta$ID %in% .self$id_imported)){
			stop('Data for one or more ID has not yet been imported')
		}
		
		stopifnot(is.logical(gam) && length(gam)==1)
		stopifnot(is.character(milking) && length(milking)==1)
		
		ald <- c('none','fixed','adjusted')
		milking <- pmatch(milking, ald)
		if(is.na(milking)){
			stop('The milking option must take one of the three following values:  none, fixed, adjusted')
		}
		milking <- ald[milking]
		
		# Add new columns to the data:
		meandate <- mean(as.numeric(.self$Data$Date))
		.self$Data <- .self$Data %>% 
			mutate(Day=as.numeric(.data$Date)-meandate, daily=as.numeric(NA), 
				milking=as.numeric(NA), prediction=as.numeric(NA), residual=as.numeric(NA))
		
		# Get daily frequency for all individuals:
		.self$Data$daily <- 2*pi*toprop(.self$Data$Time)
		stopifnot(all(!is.na(.self$Data$daily)) && all(.self$Data$daily >=0) && all(.self$Data$daily <=2*pi))
			
		# Set up lists to save between-day (i.e. once per day) and within-day (i.e. one day) for each individual
		between_day <- vector('list', length=length(.self$Meta$ID))
		names(between_day) <- .self$Meta$ID
		within_day <- vector('list', length=length(.self$Meta$ID))
		names(within_day) <- .self$Meta$ID
		
		cat('Running analyses for', length(.self$Meta$ID), 'individuals...\n')
		pb <- txtProgressBar(style=3)
		for(i in seq_along(.self$Meta$ID)){
			
			## Extract relevant data:
			tmeta <- .self$Meta[i,]
			tID <- tmeta$ID
			din <- which(.self$Data$ID == tID)
			
			# Get milking times/frequencies for this individual:
			breakprops <- interpret_times(tmeta$Milking, proportions=TRUE, warn=FALSE)[[1]]
			if(length(breakprops)==0){
				.self$Data$milking[din] <- 0
			}else{
				
				prop <- toprop(.self$Data$Time[din])
				freq <- length(breakprops)
				stopifnot(freq >= 2)
				
				if(milking=='fixed'){
					## Non-adjusted milking frequency:
					.self$Data$milking_fixed[din] <- 2*pi*(prop*freq - floor(prop*freq))
				}else if(milking=='adjusted'){
					## Adjusted milking frequency:
					# Need to add the first breakpoints outside the range as well so the proportion can be determined:
					breakprops <- c(breakprops[length(breakprops)]-1, breakprops, breakprops[1]+1)
					stopifnot(isTRUE(all.equal(breakprops[2]-breakprops[1], breakprops[length(breakprops)]-breakprops[length(breakprops)-1])))
					# Now re-scale the proportions relative to the progress within these breakpoints:
					for(cp in 1:(freq+1)){
						using <- prop > breakprops[cp] & prop <= breakprops[cp+1]
						.self$Data$milking[din][using] <- 2*pi*((prop[using]-breakprops[cp])/(breakprops[cp+1]-breakprops[cp]))
					}
				}else{
					stop('Error in matching milking type')
				}
			}
			stopifnot(all(!is.na(.self$Data$milking[din])) && all(.self$Data$milking[din] >=0) 
				&& all(.self$Data$milking[din] <=2*pi))
			
			# Check proportion calculations visually:
			if(FALSE){
				plotdata <- .self$Data[din,] %>% filter(.data$Date > min(.data$Date), .data$Date < (min(.data$Date)+5))
				plot(plotdata$Time, plotdata$daily, type='l')
				lines(plotdata$Time, plotdata$milking, col='blue')
			}
			
			
			## Fit the overall model for this animal:
			mdat <- .self$Data %>% filter(.data$ID==tID, .data$Date >= tmeta$Start, .data$Date <= tmeta$End)
			if(gam){
				if(milking=='none'){
					model <- gam(pH ~ sin(daily) + cos(daily) + s(Day), data=mdat)
				}else{
					model <- gam(pH ~ sin(daily) + cos(daily) + sin(milking) + cos(milking) + s(Day), data=mdat)
				}
				# Get the GAM prediction:
				bdpred <- data.frame(ID=tID, Date=min(.self$Data$Date[din]) + 0:difftime(max(.self$Data$Date[din]),min(.self$Data$Date[din]),units='days')) %>%
					mutate(Day=as.numeric(.data$Date)-meandate, daily=0, milking=0)
				bdpred$GAMpred <- predict(model, newdata=bdpred)
				bdpred <- bdpred %>% select(.data$ID, .data$Date, .data$GAMpred)
			}else{
				if(milking=='none'){
					model <- lm(pH ~ sin(daily) + cos(daily), data=mdat)
				}else{
					model <- lm(pH ~ sin(daily) + cos(daily) + sin(milking) + cos(milking), data=mdat)
				}
				# Equivalent to GAM prediction:
				bdpred <- data.frame(ID=tID, Date=min(.self$Data$Date[din]) + 0:difftime(max(.self$Data$Date[din]),min(.self$Data$Date[din]),units='days'), GAMpred=as.numeric(coef(model)['(Intercept)']))
			}
			
			# TODO:  add summary stats from main data and merge
			between_day[[tID]] <- bdpred %>% select(.data$ID, .data$Date, .data$GAMpred)
			
			# TODO:  add more summary stats e.g. range etc for particular time of day
			# Predict at 15 minute intervals (allow user to change default??):
			wdpred <- data.frame(ID=tID, prop=seq(0,2*pi,length.out=97)[-97])
			# TODO:  add milking etc and make predictions (for the closest day to the average GAM)
			within_day[[tID]] <- wdpred
			
			
			## Re-fit the model with omitted data for day +/- X days:
			alldates <- tmeta$Start + (0:difftime(tmeta$End, tmeta$Start, units='days'))			
			for(di in seq_along(alldates)){
				
				date <- alldates[di]
				
				excldates <- date
				if(exclude_pre > 0){
					excldates <- c(excldates, date-(1:exclude_pre))
				}
				if(exclude_post > 0){
					excldates <- c(excldates, date+(1:exclude_post))
				}
				
				mdat <- .self$Data %>% filter(.data$ID==tID, .data$Date >= tmeta$Start, .data$Date <= tmeta$End, !.data$Date %in% excldates)
				if(gam){
					if(milking=='none'){
						model <- gam(pH ~ sin(daily) + cos(daily) + s(Day), data=mdat)
					}else{
						model <- gam(pH ~ sin(daily) + cos(daily) + sin(milking) + cos(milking) + s(Day), data=mdat)
					}
				}else{
					if(milking=='none'){
						model <- lm(pH ~ sin(daily) + cos(daily), data=mdat)
					}else{
						model <- lm(pH ~ sin(daily) + cos(daily) + sin(milking) + cos(milking), data=mdat)
					}
				}
				fakedata <- .self$Data %>% filter(.data$ID==tID, .data$Date == date)
				dpred <- predict(model, newdata=fakedata)
				.self$Data$prediction[.self$Data$ID==tID & .self$Data$Date==date] <- dpred
			}
			
			setTxtProgressBar(pb, i/length(.self$Meta$ID))
		}
		close(pb)
		
		.self$Daily <- do.call('rbind', between_day)
		rownames(.self$Daily) <- NULL
		.self$Hourly <- do.call('rbind', within_day)
		rownames(.self$Hourly) <- NULL
	}
	
	
#   TODO: Plot method for looking at graphical summaries of one/more animals, and saving to file?

#   TODO: Analyse method generates prediction within window, and daily data and overview (sine curve stats) - gam or not

#   TODO: Retrieve methods to get data frames and save CSV files	
	
))
