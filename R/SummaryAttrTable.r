#' Summary statistics for an attribute table
#'
#' Calculate some simple statistics based on the attribute table. 
#' The purpose of this function is mostly to enable easy checks of things
#' like total cell numbers and types of elements.
#' 
#' @param AttrTable data.frame An attribute table cleaned using \code{\link{CleanAttrTable}}
#' @return Prints the results to the screen
#' @export

SummaryAttrTable = function(AttrTable) {
	ncells = sum(AttrTable$NumberOfCells)
	elements = sort(unique(AttrTable$ElementType))
	cat('--------------------------------------------\n')
	cat('Total number of cells in raster:', ncells, '\n')
	cat('--------------------------------------------\n')
	cat('The elements are:\n')
	cat(elements, '\n')
}