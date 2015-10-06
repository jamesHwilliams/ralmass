#' Remove comma as thousand separator
#'
#' Some tables get exported from ArcGis using commas as thousand separator
#' and are therefore imported as character. This function simply removes the 
#' commas.
#' 
#' @param dt data.table The data.table with the column containing commas.
#' @param column character Name of the column with commas.
#' @return The data.table with the comma issue fixed.
#' @export
FixSeparator = function(dt = NULL, column = NULL) {
	if(is.null(column)) {
		stop('column name missing')
	}
	if(is.null(dt)){
		stop('data.table missing')
	}
	varname = as.name(column)
	r = eval(varname, dt)
	newcol = gsub(pattern = ',', replacement = '', x = dt[, r], fixed = FALSE)
	dt[, match(as.character(varname), names(dt)):=newcol]
	return(dt)
}
