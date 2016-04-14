#' Check densities of hunters per farm
#'
#' Do a check of the densities of hunters per farm. 
#' This is just ensuring that the density criteria is not broken.
#' 
#' 
#' @param data data.table The file HuntingLocationsFarm.txt
#' @param maxdensity numeric The density paramter setting
#' @param area character The name of the column on which the area calculation
#' should be based.
#' @return logical TRUE if all densities legal, FALSE otherwise
#' @export
CheckDensity = function(data = NULL, maxdensity = NULL, area = NULL) 
{
	if(any(is.null(data), is.null(maxdensity), is.null(area)))
	{
		stop('Input parameter missing')
	}
	vals = eval(as.name(area), farms)
	densities = data[,NoHunters]/(vals/10000)
	return(all(densities <= maxdensity))
}

