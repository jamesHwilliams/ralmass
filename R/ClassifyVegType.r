#' Classify fields based on the last digit in the farm reference number
#'
#' Classify fields based on the last digit in the farm reference number
#' 
#' List explaining the translation from one digit to the ALMaSS landscape element code:
#'\itemize{
#'   \item 0 = 20
#'   \item 1 = 27
#'   \item 2 = 33
#'   \item 3 = 35
#'   \item 4 = 55
#'   \item 5 = 56
#'   \item 6 = 59
#'   \item 7 = 60
#'   \item 8 = 71
#'   \item 9 = 68
#'  } 
#' 
#' @param polyref data.frame A dataframe which was produced using \code{\link{CleanAttrTable}}.
#' @param firstfield numeric The first elementtype number which is a field belonging to a farm.
#' @return A data.frame with the same columns as in the input, but with the element types
#' corrected and the farmref stripped of its last digit (the field type identifier).
#' @export

ClassifyFields = function(polyref, firstfield = 100000) {
# Move the farmIDs to the farmref column and translate codes:
	fieldindex = which(polyref$ElementType >= firstfield)
	polyref[fieldindex,]$Farmref = polyref[fieldindex,]$ElementType
	polyref$temp = rep(NA, nrow(polyref))
	polyref[fieldindex,]$temp = as.numeric(str_sub(polyref[fieldindex,]$Farmref, -1))

	refDigits = unique(as.numeric(str_sub(polyref[fieldindex,]$Farmref, -1)))  # Take only the last digit (the field type)
	tolecodes = c(20, 27, 33, 35, 55, 56, 59, 60, 71, 68)
	for (i in refDigits) {
		index = which(polyref$temp == i)
		polyref[index,]$ElementType = tolecodes[i+1]  # The +1 is needed as the refDigits goes from 0
	}

# Get rid of the temp column:
	polyref = polyref[-match('temp', names(polyref))]
# Strip off the last digit (the element code)
	polyref[fieldindex,]$Farmref = as.numeric(str_sub(polyref[fieldindex,]$Farmref, 1, 5))
	return(polyref)
}