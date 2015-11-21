#' Check densities of hunters per farm
#'
#' Do a check of the densities of hunters per farm. 
#' This is just ensuring that the density criteria is not broken.
#' 
#' 
#' @param data data.table The file HuntingLocationsFarm.txt
#' @param MaxDensity numeric The density paramter setting
#' @return logical TRUE if all densities legal, FALSE otherwise
#' @export
CheckDensity = function(data = NULL, MaxDensity = NULL) 
{
	if(any(is.null(data), is.null(MaxDensity)))
	{
		stop('Input parameter missing')
	}
	AnyRows = nrow(data[Numbers > MaxDensity]) != 0
	if(AnyRows != 0)
	{
		return(FALSE)
	}
	if(AnyRows == 0)
	{
		return(TRUE)
	}
}

