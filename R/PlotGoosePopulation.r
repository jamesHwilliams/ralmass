#' Plot goose population data
#'
#' Plot the number of geese per species and type
#' 
#' @param data data.frame or data.table The ALMaSS data file GoosePopulationData.txt
#' @param type character The plot type. Either 'point' or 'line'.
#' @return A nice plot
#' @export
PlotGoosePopulation = function(data, type = 'point'){
	melted = data.table::melt(data, id.vars = 'Day',
		variable.name = 'GooseType', value.name = 'Numbers')
	melted = melted[Numbers != 0,]
	
	pop = ggplot2::ggplot(melted, ggplot2::aes(Day, Numbers, group = GooseType)) + 
	theme_bw()

	if(tolower(type) == 'point') 
	{
		pop = pop + ggplot2::geom_point(aes(colour = GooseType)) +
		ggplot2::ylab('Numbers')
	}
	if(tolower(type) != 'point') 
	{
		pop = pop + ggplot2::geom_line(aes(colour = GooseType)) + 
			ggplot2::geom_point(aes(colour = GooseType)) + 
			ggplot2::ylab('Numbers')
	}
	return(pop)
}


