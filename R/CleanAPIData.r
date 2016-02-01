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
	if(!is.data.table(APIData)){
		APIData = data.table::as.data.table(APIData)
	}
	APIData = APIData[CNTRY_NAME == 'Denmark', .(OBS_DATE, SEXE, API)]
	data.table::setnames(APIData, c('Date', 'Sex', 'API'))
	APIData[, API:=as.numeric(API)]
	APIData[, Date:=lubridate::ymd(Date)]
	APIData = APIData[complete.cases(APIData)]
	APIData[Sex == 'M', Weight:=2473+197.1*API]
	APIData[Sex == 'F', Weight:=2291+185.4*API]
	APIData = APIData[Sex %in% c('F', 'M'),]  # Sometimes strange characters 
	# instead of just missing data, so complete.cases is not enough.
	return(APIData)
}
