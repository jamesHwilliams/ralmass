#' Check densities of hunters per farm
#'
#' Do a check of the densities of hunters per farm. 
#' This is just ensuring that the density criteria is not broken.
#' 
#' 
#' @param data data.table The file HuntingLocationsFarm.txt
#' @param maxdensity numeric The density paramter setting
#' @param colname character The name of the column with the deinsities to check
#' @return logical TRUE if all densities legal, FALSE otherwise
#' @export
CheckDensity = function(data = NULL, maxdensity = NULL, colname = NULL) 
{
	if(any(is.null(data), is.null(maxdensity), is.null(colname)))
	{
		stop('Input parameter missing')
	}
	vals = eval(as.name(colname), farms)
	return(all(vals <= maxdensity))
}

