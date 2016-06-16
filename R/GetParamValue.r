#' Get the value of a config variable
#'
#' Return the value of a config variable in a specified ALMaSS config file.
#' 
#' @param config character Path to the file to read start and leave date from
#' @param param character The name of the config variables
#' @return The value of the config parameter
#' @export 
	GetParamValue = function(config = NULL, param = NULL) {
		if(any(is.null(config), is.null(param))){
			stop('Input parameter missing')
		}
	value = config[grep(param, config)] 
	valuestring = stringr::str_split(value[1], '=')
	value = as.numeric(stringr::str_trim(valuestring[[1]][2]))
	return(value)
}
