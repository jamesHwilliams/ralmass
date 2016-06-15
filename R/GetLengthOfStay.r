#' Get the length of stay of geese in the simulation
#'
#' Read the config file and figure out how many days geese should stay in the 
#' simulation. The answer is corrected for the situation where the model exit
#' day is set before the leave date for the goose species in question. 
#' 
#' @param config character Path to the file to read start and leave date from
#' @param species character The species to the length of stay for
#' @return numeric The length of the stay
#' @export
GetLengthOfStay = function(config = NULL, species = NULL) {
	if(tolower(species) == 'greylag'){ sp = 'GL'}
	if(tolower(species) == 'pinkfoot'){ sp = 'PF'}
	if(tolower(species) == 'barnacle'){ sp = 'BN'}
	# Get leavedate
	matchstring = paste('GOOSE', sp, 'LEAVINGDATEEND', sep = '_')
	leavedate = GetParamValue(config, matchstring)
	# Get startdate
	matchstring = paste('GOOSE', sp, 'ARRIVEDATESTART', sep = '_')
	startdate = GetParamValue(config, matchstring)
	# Get exitday
	exitday = GetParamValue(config, 'MODELEXITDAY')
	exitday = exitday %% 365  # To get day in year
	if(leavedate < exitday) 
	{
		lengthofstay = 366-startdate + leavedate
	}
	if(leavedate >= exitday) 
	{
		lengthofstay = 366-startdate + exitday
	}
	return(lengthofstay)
}
#' @param config character Path to the file to read start and leave date from
#' @param param character The name of the config variables
#' @export 
	GetParamValue = function(config, param) {
	value = config[grep(param, config)] 
	valuestring = stringr::str_split(value[1], '=')
	value = as.numeric(stringr::str_trim(valuestring[[1]][2]))
	return(value)
}