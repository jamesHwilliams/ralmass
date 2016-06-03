#' Write reclass table
#'
#' Write a reclassification table to be used with the arcpy tool
#'  ReclassByASCIIFile. 
#'
#' @param Table data.table A two column table where the first column is the 
#' polyref number from the ALMaSS landscape. The second column is the entity
#' to be mapped e.g goose numbers.
#' @param PathToFile character The path to the where the file is 
#' written (including the name and extension of the file, .txt)
#' @export
WriteReclassTable = function(Table, PathToFile) {
	if(any(is.null(Table), is.null(PathToFile))) {
		stop('Input argument missing')
	} 
	write.table(Table, file = PathToFile, sep = ' : ', row.names = FALSE,
	 col.names = FALSE, quote = FALSE)
}
