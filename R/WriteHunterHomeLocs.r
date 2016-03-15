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
	filecon = file(PathToFile, open = 'wt')
	cat(paste(nrow(Locs)), '\n',  file = filecon)
	ScipenDefault = getOption('scipen')
	options(scipen = 99)  # To avoid scientific notation in the resulting file
	write.table(Locs, file = filecon, sep = '\t', append = TRUE,
	 row.names = FALSE, col.names = FALSE, quote = FALSE)
	close(filecon)
	options(scipen = ScipenDefault)  # Reset scipen option to default
}