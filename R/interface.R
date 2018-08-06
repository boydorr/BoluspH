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
	fields = list(Meta='data.frame', Data='data.frame', Overview='data.frame', Daily='data.frame', id_imported='character'), 

	methods = list(

	initialize = function(meta_data=data.frame(ID=character(0), Filename=character(0), Milking=character(0))){
		"Set up a BoluspH object (with meta data) before importing files"
		
		if(!is.data.frame(meta_data) || !all(c('ID','Filename','Milking') %in% names(meta_data)) || nrow(meta_data) < 1){
			stop('The argument to meta_data must be a data frame including the columns ID (giving a unique identifier to be used for the cow), Filename (giving the filename used for the corresponding cow), and Milking (giving a character string with comma separated times of day, or a blank character string).  Other columns may also be used if required for later analysis.')
		}
		
		if(length(unique(meta_data$ID))!=nrow(meta_data))
			stop('Non-unique ID detected in the meta data')

		if(length(unique(meta_data$Filename))!=nrow(meta_data))
			stop('Non-unique Filename detected in the meta data')
		
		# Check/Interpret Milking times:
		tm <- meta_data$Milking
		tochange <- tm!=''
		meta_data$Milking <- interpret_times(meta_data$Milking, proportions=FALSE, warn=TRUE)
		
		.self$Meta <- meta_data %>% select(.data$ID, .data$Filename, .data$Milking, everything())
		.self$Data <- data.frame(ID=factor(levels=.self$Meta$ID), Date=as.Date(character(0)), Time=as.POSIXct(character(0), tz='GMT'), pH=numeric(0))
		
	},
	
	AddCustomCSVData = function(folder_path, skip=1, date_col=1, time_col=2, pH_col=4, sep=',', dec='.', date_format='%Y-%m-%d', time_format='%H:%M'){
		"Import one or more rumen pH bolus data CSV files using a custom file format"
		
		if(!is.character(folder_path) || length(folder_path)!=1){
			stop('The argument to folder_path must be a single path to a folder containing CSV files matching the Filenames in the meta-data (to be imported)')
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
		
		files <- get_files(folder_path, extension="csv", targets=.self$Meta$Filename[!.self$Meta$ID %in% .self$id_imported])
		cat('Importing ', nrow(files), ' CSV files...\n')
		newdata <- vector('list', length=nrow(files))
		for(i in seq_len(nrow(files))){
			cat('\tReading: ', files$path[i], '\n')
			ID <- .self$Meta$ID[.self$Meta$Filename==files$Filename[i]]
			stopifnot(length(ID)==1)
			.self$id_imported <- c(.self$id_imported, ID)
			newdata[[i]] <- read_csv_file(path=files$path[i], skip=skip, date_col=date_col, time_col=time_col, pH_col=pH_col, sep=sep, dec=dec, date_format=date_format, time_format=time_format, ID=ID)
		}
		cat('\tDone\n')
		
		.self$Data <- rbind(.self$Data, do.call('rbind', newdata))
		
	}
	
#	TODO: Window method for changing start and end dates

#   TODO: Plot method for looking at graphical summaries of one/more animals, and saving to file?

#   TODO: Analyse method generates prediction within window, and daily data and overview (sine curve stats) - gam or not

#   TODO: Retrieve methods to get data frames and save CSV files	
	
))
