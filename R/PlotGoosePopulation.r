#' Plot goose population data
#'
#' Plot the number of geese per species and type
#'
#' @param data data.table The ALMaSS data file GoosePopulationData.txt
#' @param dates logical Should we plot day numbers as dates?
#' @return A nice plot
#' @export
PlotGoosePopulation = function(data, dates = FALSE){
	melted = data.table::melt(data, id.vars = c('Day', 'SeasonNumber'),
		variable.name = 'GooseType', value.name = 'Numbers')
	melted = melted[Numbers != 0,]
	if(dates) {
	melted[, Day:=as.Date(Day, origin = '2010-01-01')]
	}
	p = ggplot2::ggplot(melted, ggplot2::aes(Day, Numbers, group = GooseType)) +
		ggplot2::geom_line(ggplot2::aes(colour = GooseType)) +
		ggplot2::geom_point(ggplot2::aes(colour = GooseType)) +
		ggplot2::ylab('Numbers') + ggplot2::theme_bw()
	if(dates) {
	  p = p + ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
	    ggplot2::xlab('Month')
	}
	return(p + scale_color_brewer(palette = 'Paired'))
}


