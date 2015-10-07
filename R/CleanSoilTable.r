#' Clean soil type tables written from arcpy.
#'
#' The function will remove commas used as thousand seperator, re-arrange columns and add missing ones. 
#' 
#' @param SoilTable data.table An table with soil types exported from arcpy
#' @return A clean version of the soil type table with the correct dimension
#' for use in ALMaSS.
#' @export
CleanSoilTable = function(SoilTable) {
	if(!is.data.table(SoilTable)){
		stop('Input needs to be a data.table')
	}
	SoilTable[, c('OID', 'COUNT', 'AREA'):=NULL]
	setnames(SoilTable, c('PolyRefNum', 'SoilType'))
	if(is.character(SoilTable$PolyRefNum))
	{
		SoilTable$PolyRefNum = gsub(pattern = ',', replacement = '', x = SoilTable$PolyRefNum, fixed = FALSE)
		SoilTable$PolyRefNum = as.numeric(SoilTable$PolyRefNum)
	}
	setkey(SoilTable, PolyRefNum)
	return(SoilTable)
}