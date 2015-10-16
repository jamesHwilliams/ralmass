#' Check the frequency of farm reference numbers
#'
#' Do a check of the frequencies of individual farm reference numbers.
#' The function summarise the frequency across the hunting locations and return
#' a table ready to plot. 
#' 
#' @param resdir character The path to the hunting locations file
#' @return data.table a table with the frequencies of individual farm 
#' reference numbers.
#' @export
CheckFarmRefFreq = function(resdir = NULL) {
	if(is.null(resdir)){
		stop('Input parameter missing')	
	} 
	resfiles = dir(resdir)
	resfileindex = grep('HuntingLocationsRun', resfiles)	
	loclist = vector('list', length(resfileindex))
	# Define function to summarise frequencies
	fun = function(loc) {
		loc[, Freq1:= .N, by = 'FarmRef1']
		loc[, Freq2:= .N, by = 'FarmRef2']
		loc[, Freq3:= .N, by = 'FarmRef3']
		loc[, Freq4:= .N, by = 'FarmRef4']
		loc[, Freq5:= .N, by = 'FarmRef5']
		farmvect = c(loc[,FarmRef1], loc[,FarmRef2], loc[,FarmRef3],
			loc[,FarmRef4], loc[,FarmRef5])
		freqvect = c(loc[,Freq1],loc[,Freq2],loc[,Freq3],loc[,Freq4],loc[,Freq5])
		refs = c('FarmRef1', 'FarmRef2', 'FarmRef3', 'FarmRef4', 'FarmRef5')
		refs = rep(refs, each = nrow(loc))
		tab = as.data.table(cbind(farmvect, freqvect, refs))
		setnames(tab, c('FarmRef', 'Freq', 'Refs'))
		tab[, FarmRef:=as.numeric(FarmRef)]
		tab[, Freq:=as.numeric(Freq)]
		tab = tab[!is.na(FarmRef),]
		tab = unique(tab)
		return(tab)
	}
	# Apply across list
	for (i in resfileindex) {
		thelocfile = fread(paste0(resdir, resfiles[i]))
		loclist[[i]] = fun(thelocfile)
	}
	list = unlist(loclist, recursive = FALSE)
	dt = do.call("rbind", loclist)
	return(dt)
}

