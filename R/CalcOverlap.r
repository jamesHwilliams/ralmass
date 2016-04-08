#' Calculate the overlap of kernel densities
#'
#' Calculate the overlap of kernel densities. Either densities of flock sizes
#' calculated from field observations and from the simulated flock sizes, of 
#' hunter densities on farms or kernel densities of number of hunters per farm
#' 
#' This function is based on the answer to this CV question: 
#' http://stats.stackexchange.com/questions/97596/how-to-calculate-overlap
#'-between-empirical-probability-densities
#' 
#' @param data data.table An object containing both the simulated and observed 
#' flock sizes and the three columns 'Species', 'Numbers' and 'Type'
#' @param species character The species for which the calculated for.
#' Either 'Barnacle', 'Pinkfoot','Greylag' or 'Hunter'
#' @return numeric The overlap
#' @export
CalcOverlap = function(data = NULL, species = NULL) 
{
	if(any(is.null(data), is.null(species))) {
		stop('Input argument missing \n')
	}
	if(tolower(species) != 'hunter'){
		species = stringr::str_to_title(species)  # Ensuring species has the 
												  # right case.
		var = c(species, paste0(species, 'Timed'))
		data = data[Species %in% var,][,Numbers:=log10(Numbers)]
		ndatatypes = length(unique(data[,Type]))
		if(ndatatypes < 2) 
		{
			return(0)
		}
	}

  # The actual calculation is based on this CV question: 
  # http://stats.stackexchange.com/questions/97596/how-to-calculate-overlap-between-empirical-probability-densities
  # Set limits of a common grid, ensuring that tails aren't cut off
	min = min(data[, Numbers]) - 1
	max = max(data[, Numbers]) + 1

	simdens = density(data[Type == 'Simulated', Numbers], from=min, to=max)
	obsdens = density(data[Type == 'Fieldobs', Numbers], from=min, to=max)
	d = data.frame(x=simdens$x, a=simdens$y, b=obsdens$y)

  # calculate intersection densities
	d$w = pmin(d$a, d$b)

  # Integrate the area under the curves
	total = sfsmisc::integrate.xy(d$x, d$a) + sfsmisc::integrate.xy(d$x, d$b)
	intersection = sfsmisc::integrate.xy(d$x, d$w)

	return(2 * intersection / total)
}

