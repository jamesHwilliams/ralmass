#' Get the length of stay of geese in the simulation
#'
#' Read the config file and figure out how many days geese should stay in the 
#' simulation.
#' 
#' @param config character Path to the file to read start and  leave date from
#' @return numeric The length of the stay
#' @export
GetLengthOfStay = function(config = NULL, species = NULL) {
	if(tolower(species) == 'greylag'){ sp = 'GL'}
	if(tolower(species) == 'pinkfoot'){ sp = 'PF'}
	if(tolower(species) == 'barnacle'){ sp = 'BN'}
	matchstring = paste('GOOSE', sp, 'LEAVINGDATEEND', sep = '_')
	leavedate = cfg[grep(matchstring, cfg)] 
	leavestrings = stringr::str_split(leavedate[1], '=')
	leavedate = as.numeric(stringr::str_trim(leavestrings[[1]][2]))
	matchstring = paste('GOOSE', sp, 'ARRIVEDATESTART', sep = '_')
	startdate = cfg[grep(matchstring, cfg)] 
	startstrings = stringr::str_split(startdate[1], '=')
	startdate = as.numeric(stringr::str_trim(startstrings[[1]][2]))
	lengthofstay = 366-startdate + leavedate
	return(lengthofstay)
}
