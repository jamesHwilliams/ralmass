#' Write hunter home location file
#'
#' Write a file with hunter home locations
#'
#' @param Locs data.frame or data.table The object with home locations
#' @param PathToFile character The path to the where the file is 
#' written (including the name and extension of the file, usually .txt)
#' @param Headers logical Should the output file include headers?
#' @return A tab separated text file formatted according to the requirements
#'  for a hunter home location file for ALMaSS.
#' @export
WriteHunterHomeLocs = function(Locs = NULL, PathToFile = NULL){
	if(any(is.null(Locs), is.null(PathToFile))) {
		stop('Input missing')
	}
	if(!is.data.table(Locs)) 
	{
		Locs = as.data.table(Locs)
	}
	rows = nrow(Locs)
	filecon = file(PathToFile, open = 'wt')
	cat(paste(rows), '\n',  file = filecon)
	ScipenDefault = getOption('scipen')
	options(scipen = 99)  # To avoid scientific notation in the resulting file
	Locs[,ID:=0:(rows-1)]
	setcolorder(Locs, neworder = c(3,1,2))
	write.table(Locs, file = filecon, sep = '\t', append = TRUE,
	 row.names = FALSE, col.names = FALSE, quote = FALSE)
	close(filecon)
	options(scipen = ScipenDefault)  # Reset scipen option to default
}