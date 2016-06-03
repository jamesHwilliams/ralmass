#' Plot goose population data
#'
#' Plot the number of geese per species and type
#' 
#' @param data data.table The ALMaSS data file GoosePopulationData.txt
#' @return A nice plot
#' @export
PlotGoosePopulation = function(data){
	melted = data.table::melt(data, id.vars = c('Day', 'SeasonNumber'),
		variable.name = 'GooseType', value.name = 'Numbers')
	melted = melted[Numbers != 0,]
	
	p = ggplot2::ggplot(melted, ggplot2::aes(Day, Numbers, group = GooseType)) + 
		ggplot2::geom_line(ggplot2::aes(colour = GooseType)) + 
		ggplot2::geom_point(ggplot2::aes(colour = GooseType)) + 
		ggplot2::ylab('Numbers') + ggplot2::theme_bw()
	return(p)
}


