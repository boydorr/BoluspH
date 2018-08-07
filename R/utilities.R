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
	
	if(!proportions){
		# Return to simple character vector:
		rv <- sapply(rv, paste, collapse=',')
	}
	
	return(rv)
}

toprop <- function(time){
	stopifnot(inherits(time, 'POSIXct'))
	prop <- as.numeric(strftime(time, format='%H'))/24 + as.numeric(strftime(time, format='%M'))/(24*60) + as.numeric(strftime(time, format='%S'))/(24*60*60)
	return(prop)
}