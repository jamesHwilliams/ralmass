#' Generate a range of paramter values
#'
#' Generate a range of paramter values from specified start 
#' and end values. A simple wrapper around seq
#' 
#' 
#' @param start numeric Starting value.
#' @param end numeric End value.
#' @param N Number of required values.
#' @return data.frame Two column data.frame. First column the config variable
#' and second column being the values.
#' @export

GenerateParams = function(start, end, N, Config)
{
	val = seq(start, end, length.out = N)
	data.frame('Config' = rep(Config, N), 'Value' = val)
}

GenerateParams(0, 100, 11, 'asd')