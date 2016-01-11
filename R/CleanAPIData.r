#' Clean the API field data
#'
#' Clean API field data and calculate weights from API scores.
#' 
#' @param APIData data.table the raw field data
#' @return The cleaned up data.
#' @export
CleanAPIData = function(APIData) {
	if(is.null(APIData)){
		stop('File missing')
	}
	field = field[CNTRY_NAME == 'Denmark', .(OBS_DATE, SEXE, API)]
	data.table::setnames(field, c('Date', 'Sex', 'API'))
	field[, Date:=lubridate::dmy(Date)]
	field[Sex == 'M', Weight:=2473+197.1*API]
	field[Sex == 'F', Weight:=2291+185.4*API]
	field = field[Sex %in% c('F', 'M'),]
	return(field)
}
