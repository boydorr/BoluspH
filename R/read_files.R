interpret_times <- function(times, proportions, warn=FALSE){
	
	stopifnot(is.character(times))
	stopifnot(length(times)>0)
	
	rv <- vector('list', length=length(times))
	stimes <- strsplit(times, split=',')
	for(i in seq_along(times)){
		nt <- stimes[[i]]
		if(warn && length(nt) == 1 && nt[1]!=''){
			cat('Note: ignoring single milking event\n')
		}
		if(length(nt)>1){
			nt[!grepl(':',nt)] <- paste0(nt[!grepl(':',nt)], ':00')
			## Just for checking:
			for(j in seq_along(nt)){
				ss <- try(ndt <- as.POSIXct(paste0('2000-01-01 ', nt[j]), tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M"), tz='GMT'), silent=TRUE)
				if(inherits(ss, 'try-error')){
					stop('Unrecognised milking time format: ', nt[j])
				}				
			}
			## Re-get the result as a vector:
			ndt <- as.POSIXct(paste0('2000-01-01 ', nt), tryFormats = c("%Y-%m-%d %H:%M:%OS", "%Y-%m-%d %H:%M"), tz='GMT')
			if(proportions){
				hm <- cbind(as.numeric(strftime(ndt, format='%H', tz='GMT')), as.numeric(strftime(ndt, format='%M', tz='GMT')))
				rv[[i]] <- c((hm[,1]+hm[,2]/60)/24)
			}else{
				rv[[i]] <- strftime(ndt, format='%H:%M', tz='GMT')
			}
		}else if(proportions){
			rv[[i]] <- numeric(0)
		}else{
			rv[[i]] <- ''
		}
	}
	
	return(rv)
}

get_files <- function(folder_path, extension, targets){
	
	stopifnot(length(folder_path)==1)
	if(!file.exists(folder_path) || !file.info(folder_path)$isdir){
		stop('The speficied folder_path "', folder_path, '" either does not exist or is not a directory')
	}
	
	## Find files:
	files <- list.files(folder_path, pattern=paste0('\\.', extension, '$'))
	if(length(files)==0){
		stop('No files with extension ".', extension, "' found in the specified folder path")
	}

	## If the target filename doesn't already end with this extension then try adding it:
	newtargets <- ifelse(grepl(paste0('\\.', extension, '$'), targets), targets, paste0(targets[!grepl(paste0('\\.', extension, '$'), targets)], '.', extension))
	
	## Only match exactly:
	file_in <- na.omit(match(files, newtargets))
	return(data.frame(Filename=targets[file_in], path=file.path(folder_path, newtargets[file_in]), stringsAsFactors=FALSE))
	
}

read_csv_file <- function(path, skip, date_col, time_col, pH_col, sep, dec, date_format, time_format, ID){
	
	dat <- read.table(path, header=FALSE, sep=sep, dec=dec, skip=skip)
	if(ncol(dat) < max(c(date_col, time_col, pH_col)))
		stop('Unable to read CSV file ', path, ' as the number of columns (', ncol(dat), ') is less than max(c(date_col, time_col, pH_col))')
	
	dat <- data.frame(ID=ID, Date=dat[,date_col], Time=dat[,time_col], pH=dat[,pH_col], stringsAsFactors=FALSE)

	tt <- dat$Date[1]
	dat$Date <- as.Date(dat$Date, format=date_format, tz='GMT')
	if(any(is.na(dat$Date))){
		stop('Missing dates generated using specified format: ', date_format, ' - first observed date is: ', tt)
	}
	
	# If the time does not already contain the year then presume it is missing the date:
	tt <- dat$Time[1]
	orig_time_format <- time_format
	if(!grepl('%Y', time_format) || !grepl('%y', time_format)){
		dat$Time <- paste(strftime(dat$Date, format='%Y-%m-%d', tz='GMT'), dat$Time)
		time_format <- paste('%Y-%m-%d', time_format)
	}
	dat$Time <- as.POSIXct(dat$Time, format=time_format, tz='GMT')
	if(any(is.na(dat$Time))){
		stop('Missing times generated using specified format: ', orig_time_format, ' - first observed time is: ', tt)
	}
	
	tt <- dat$pH[1]
	dat$pH <- as.numeric(dat$pH)
	if(any(is.na(dat$pH))){
		stop('Missing pH values generated using specified dec: ', dec, ' - first observed pH is: ', tt)
	}
	
	return(dat)
}
