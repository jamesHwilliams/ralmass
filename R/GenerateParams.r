#' Generate a range of paramter values
#'
#' Generate a range of paramter values from specified start 
#' and end values. A simple wrapper around seq
#' 
#' 
#' @param start numeric Starting value.
#' @param end numeric End value.
#' @param N Number of required values.
#' @param Config The character string with the config variable (including (int))
#' @param start2 numeric Starting value for the (optional) second variable
#' @param end2 numeric End value for the (optional) second variable
#' @param N2 Number of required values for the (optional) second variable
#' @param Config2 The character string with the second config variable (including (int))
#' @return data.frame One column each value a text string with the config and 
#' and its value
#' @export

GenerateParams = function(start, end, N, Config,
 start2 = NULL, end2 = NULL, N2 = NULL, Config2 = NULL,
 write = FALSE)
{
	if(is.null(start2) | is.null(end2) | is.null(N2) | is.null(Config2))
	{
		val = seq(start, end, length.out = N)
		config = rep(Config, N)
		df = data.frame('Config' = paste(config, sep = ' = ', 'Value' = val))
	}
	if(!is.null(start2) | !is.null(end2) | !is.null(N2) | !is.null(Config2))
	{
		val = seq(start, end, length.out = N)
		val2 = seq(start2, end2, length.out = N2)
		a = rep(val, each = length(val2))
		b = rep(val2, length(val))
		end = rep(NA, length(a))
		uneven = seq(1, length(a)*2, by = 2)
		even = seq(2, length(a)*2, by = 2)
		end[uneven] = a
		end[even] = b
		confignames = rep(c(Config, Config2), length(a))
		df = data.frame('Params' = paste(confignames, end, sep = ' = '))
	}
	if(write)
	{
		write.table(df, file = 'ParameterValues.txt', sep = '\t', quote = FALSE,
			row.names = FALSE, col.names = FALSE)
		cat('Printed following to ParameterValues.txt:\n')
	}
	return(df)
}