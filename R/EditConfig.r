#' Edit config file
#'
#' Edit a config file with a specific config variable and value. The value
#' simply added to the end of the file.
#'
#' @param file character Full path to the work config file
#' @param config character The config to edit
#' @param value character The value of the config
#' @export
EditConfig = function(file = NULL, config = NULL, value = NULL) {
	if(any(is.null(file), is.null(config), is.null(value))) 
	{
		stop('Input argument missing')
	}
	TheConfig = GetParamType(config)
	TheLine = paste(TheConfig, '=', value, sep = ' ')
	write(TheLine, file, append = TRUE)
}
