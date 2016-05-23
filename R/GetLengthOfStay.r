#' Get the length of stay of geese in the simulation
#'
#' Read the config file and figure out how many days geese should stay in the 
#' simulation. The answer is corrected for the situation where the model exit
#' day is set before the leave date for the goose species in question. 
#' 
#' @param config character Path to the file to read start and  leave date from
#' @param species character The species to the length of stay for
#' @return numeric The length of the stay
#' @export
GetLengthOfStay = function(config = NULL, species = NULL) {
	if(tolower(species) == 'greylag'){ sp = 'GL'}
	if(tolower(species) == 'pinkfoot'){ sp = 'PF'}
	if(tolower(species) == 'barnacle'){ sp = 'BN'}
	# Get leavedate
	matchstring = paste('GOOSE', sp, 'LEAVINGDATEEND', sep = '_')
	leavedate = config[grep(matchstring, config)] 
	leavestrings = stringr::str_split(leavedate[1], '=')
	leavedate = as.numeric(stringr::str_trim(leavestrings[[1]][2]))
	# Get startdate
	matchstring = paste('GOOSE', sp, 'ARRIVEDATESTART', sep = '_')
	startdate = config[grep(matchstring, config)] 
	startstrings = stringr::str_split(startdate[1], '=')
	startdate = as.numeric(stringr::str_trim(startstrings[[1]][2]))
	# Get exitday
	exitday = config[grep('MODELEXITDAY', config)]
	exitstrings = stringr::str_split(exitday[1], '=')
	exitday = as.numeric(stringr::str_trim(exitstrings[[1]][2]))
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
