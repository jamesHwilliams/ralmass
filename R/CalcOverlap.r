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
#' @param metric character The name of the column with the metric on which 
#' to calculate the kernel density overlap
#' @return numeric The overlap
#' @export
CalcOverlap = function(data = NULL, species = NULL, metric = NULL) {
	if(any(is.null(data), is.null(species))) {
		stop('Input argument missing \n')
	}
	if(!tolower(species) %in% c('barnacle', 'greylag', 'pinkfoot', 'hunter')) {
		stop('Invalid species argument \n')
	}
	if(tolower(species) != 'hunter'){
		if(is.null(metric)) {
			stop('Input parameter metric missing')
		}
		species = stringr::str_to_title(species)  # Ensuring species has the 
												  # right case.
		var = c(species, paste0(species, 'Timed'))
		data = data[Species %in% var,]
		ndatatypes = length(unique(data[,Type]))
		if(ndatatypes < 2) 
		{
			return(0)
		}
  # The actual calculation is based on this CV question: 
  # http://stats.stackexchange.com/questions/97596/how-to-calculate-overlap-between-empirical-probability-densities
  # Set limits of a common grid, ensuring that tails aren't cut off
		themetric = as.name(metric)
		min = min(eval(themetric, data)) - 1
		max = max(eval(themetric, data)) + 1

		sim = eval(themetric, data[Type == 'Simulated',])
		simdens = density(sim, from=min, to=max)
		obs = eval(themetric, data[Type == 'Fieldobs',])
		obsdens = density(obs, from=min, to=max)
		d = data.frame(x=simdens$x, a=simdens$y, b=obsdens$y)

  # calculate intersection densities
		d$w = pmin(d$a, d$b)

  # Integrate the area under the curves
		total = sfsmisc::integrate.xy(d$x, d$a) + sfsmisc::integrate.xy(d$x, d$b)
		intersection = sfsmisc::integrate.xy(d$x, d$w)

		return(2 * intersection / total)
	}
	if(tolower(species) == 'hunter')
	{
		min = min(data[, Density]) - 1
		max = max(data[, Density]) + 1

		simdens = density(data[Type == 'Simulated', Density], from=min, to=max)
		obsdens = density(data[Type == 'Fieldobs', Density], from=min, to=max)
		d = data.frame(x=simdens$x, a=simdens$y, b=obsdens$y)

  # calculate intersection densities
		d$w = pmin(d$a, d$b)

  # Integrate the area under the curves
		total = sfsmisc::integrate.xy(d$x, d$a) + sfsmisc::integrate.xy(d$x, d$b)
		intersection = sfsmisc::integrate.xy(d$x, d$w)

		return(2 * intersection / total)
	}
}


