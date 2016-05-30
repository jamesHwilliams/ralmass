#' Copy and datestamp result files
#'
#' This function copies and renames the ParameterFittingResults.txt
#' file to a user specified location. The original file name will padded by
#' the parent directory name (leading) and the output from Sys.Date() (trailing)
#'  
#' @param InDirectory character Path to the run directories
#' @param OutDirectory character Path to the location where the files will be
#' stored.
#' @export
StoreResults = function(InDirectory = NULL, OutDirectory = NULL) {
	if(any(is.null(InDirectory), is.null(OutDirectory))) 
	{
		stop('Directory location missing')
	}
	dirs = dir(InDirectory)
	for (i in seq_along(dirs)) {
		respath = file.path(InDirectory, dirs[i], 'Results')
		fileloc = file.path(respath, 'ParameterFittingResults.txt')
		if(file.exists(fileloc)) {
			parts = stringr::str_split(basename(fileloc), '\\.')
			datestamp = paste(Sys.Date(), parts[[1]][2], sep = '.')
			filename = paste(parts[[1]][1], datestamp, sep = '_')
			fullname = paste(dirs[i], filename, sep = '_')
			newfileloc = file.path(OutDirectory, fullname)
			file.copy(from = fileloc, to = newfileloc)
		}
	}
}