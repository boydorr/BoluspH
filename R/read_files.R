get_files <- function(folder_path, extension, targets){
	
	stopifnot(length(folder_path)==1)
	if(!file.exists(folder_path) || !file.info(folder_path)$isdir){
		stop('The speficied folder_path "', folder_path, '" either does not exist or is not a directory')
	}
	
	## Find files:
	files <- unlist(lapply(extension, function(x) return(list.files(folder_path, pattern=paste0('\\.', x, '$')))))
	if(length(files)==0){
		stop('No files with extension ".', paste(extension, collapse='/'), '" found in the specified folder path')
	}

	## If the target filename doesn't already end with the first given extension then try adding it:
	newtargets <- ifelse(grepl(paste0('\\.', extension[1], '$'), targets), targets, paste0(targets[!grepl(paste0('\\.', extension[1], '$'), targets)], '.', extension[1]))
	
	## Only match exactly:
	file_in <- na.omit(match(files, newtargets))
	return(data.frame(Filename=targets[file_in], path=file.path(folder_path, newtargets[file_in]), stringsAsFactors=FALSE))
	
}

read_csv_file <- function(path, skip, date_col, time_col, pH_col, sep, dec, date_format, time_format, ID){
	
	dat <- read.table(path, header=FALSE, sep=sep, dec=dec, skip=skip, stringsAsFactors=FALSE)
	if(ncol(dat) < max(c(date_col, time_col, pH_col)))
		stop('Unable to read CSV file ', path, ' as the number of columns (', ncol(dat), ') is less than max(c(date_col, time_col, pH_col))')
	
	dat <- data.frame(ID=ID, Date=dat[,date_col], Time=dat[,time_col], pH=dat[,pH_col], stringsAsFactors=FALSE)
	
	# Remove entries with missing date, time or pH:
	dat <- dat %>%
		filter(!is.na(.data$ID), !is.na(.data$Date), !is.na(.data$Time), !is.na(.data$pH)) %>%
		filter(.data$ID!="", .data$Date!="", .data$Time!="", .data$pH!="")
	
	if(nrow(dat)<1){
		stop('No valid data in file (zero rows after removing missing or blank date, time and pH)')
	}
	
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

read_excel_file <- function(path, skip, date_col, time_col, pH_col, ID){
	
	dat <- as.data.frame(read_excel(path, sheet=1, skip=skip, col_names=FALSE))
	if(ncol(dat) < max(c(date_col, time_col, pH_col)))
		stop('Unable to read Excel file ', path, ' as the number of columns (', ncol(dat), ') is less than max(c(date_col, time_col, pH_col))')
	
	dat <- data.frame(ID=ID, Date=as.Date(dat[,date_col]), Time=dat[,time_col], pH=dat[,pH_col], stringsAsFactors=FALSE)
	
	# Remove entries with missing date, time or pH:
	dat <- dat %>%
		filter(!is.na(.data$ID), !is.na(.data$Date), !is.na(.data$Time), !is.na(.data$pH))
	
	if(nrow(dat)<1){
		stop('No valid data in file (zero rows after removing missing or blank date, time and pH)')
	}
	
	return(dat)
}

