#' Clean attribute tables written from ArcGIS.
#'
#' The function will remove commas used as thousand seperator, re-arrange columns and add missing ones. 
#' The attribute table should contain (as a minimum):
#' \itemize{
#'   \item COUNT. The number of cells with the same VALUE
#'   \item LINK. This is the actual element type codes used in ALMaSS
#'   \item VALUE. The polygon ID
#' }
#'
#' @param AttrTable data.table An attribute table containing at least the tree columns: COUNT, LINK & VALUE
#' @param Soiltype Logical. Should the output contain dummy variable for soiltype?
#' @return A clean version of the attribute table with the correct dimension for use in ALMaSS.
#' @export
#' @examples
#' data(polyref)
#' str(polyref)
#' tail(polyref)  # Note the issues with commas
#' poly2 = CleanAttrTable(polyref)
#' str(poly2)  # Only the three needed columns and in the right order
#' tail(poly2)  # Comma issue gone
CleanAttrTable = function(AttrTable, Soiltype = TRUE) {
	if(!is.data.frame(AttrTable)){
		stop('AttrTable needs to be a data.table\n')
	}

	AttrTable[,OBJECTID:=NULL]
	# VALUE = polygon ID, COUNT = number of cells in the polygon, LINK = ALMaSS element type code
	setnames(AttrTable, c('PolyRefNum', 'Area', 'PolyType'))
	if(is.character(AttrTable$Area))
	{
		AttrTable$Area = gsub(pattern = ',', replacement = '', x = AttrTable$Area, fixed = FALSE)
		AttrTable$Area = as.numeric(AttrTable$Area)
	}
	if(is.character(AttrTable$PolyType))
	{
		AttrTable$PolyType = gsub(pattern = ',', replacement = '', x = AttrTable$PolyType, fixed = FALSE)
		AttrTable$PolyType = as.numeric(AttrTable$PolyType)
	}
	if(is.character(AttrTable$PolyRefNum))
	{
		AttrTable$PolyRefNum = gsub(pattern = ',', replacement = '', x = AttrTable$PolyRefNum, fixed = FALSE)
		AttrTable$PolyRefNum = as.numeric(AttrTable$PolyRefNum)
	}

	# Rearrange columns
	setcolorder(AttrTable,c('PolyType', 'PolyRefNum', 'Area'))
	# Add the farmref column (will be overwritten later...)
	AttrTable$FarmRef = rep(-1, nrow(AttrTable))
	# Add the minus one column (just has to be there...)
	AttrTable$UnSprayedMarginRef = rep(-1, nrow(AttrTable))

	if(Soiltype){
	# Add missing columns (The minus one column and soiltype which here is just a dummy)
		AttrTable$Soiltype = rep(-1, nrow(AttrTable))
	}
	setkey(AttrTable, 'PolyRefNum')
	return(AttrTable)
}