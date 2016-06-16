#' Write polyref file
#'
#' Write a polyref file to a text file ready for loading in ALMaSS.
#'
#' @param Table data.frame or data.table An attribute table
#' @param PathToFile character The path to the where the file is 
#' written (including the name and extension of the file, usually .txt)
#' @param Headers logical Should the output file include headers?
#' @param Type character Write poly- or farm-ref file?
#' @return A tab separated text file formatted according to the requirements for a polygon reference file for ALMaSS.
#' @export

WritePolyref = function(Table, PathToFile, Headers = TRUE, Type = 'Poly'){
	if(Type != 'Poly' & Type != 'Farm') 
	{
		stop('Oops! Looks like you are asking for stuff you cant get...\n')
	}
	filecon = file(PathToFile, open = 'wt')
	cat(paste(nrow(Table)), '\n',  file = filecon)
	ScipenDefault = getOption('scipen')
	options(scipen = 99)  # To avoid scientific notation in the resulting file
	if(Type == 'Poly')
	{
		Table[,`:=`(Openness = -1, CentroidX = -1, CentroidY = -1)]
	    utils::write.table(Table, file = filecon, sep = '\t', append = TRUE, row.names = FALSE, col.names = Headers, quote = FALSE)
			}
	if(Type == 'Farm') 
	{
		utils::write.table(Table, file = filecon, sep = '\t', append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
	}
	close(filecon)
	options(scipen = ScipenDefault)  # Reset scipen option to default
}