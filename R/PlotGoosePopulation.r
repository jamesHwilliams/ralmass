#' Plot goose population data
#'
#' Plot the number of geese per species and type
#' 
#' @param data data.frame or data.table The ALMaSS data file GoosePopulationData.txt
#' @return A nice plot
#' @export

PlotGoosePopulation = function(data){
data[,SimDate:=as.Date(DayInYear, origin = as.Date(paste(Year,"-01-01", sep = '')))]
data[,c('Day', 'Month', 'Year', 'Time'):=NULL]

melted = reshape2::melt(data, id.vars = c('SimDate'),
 variable.name = 'GooseType', value.name = 'Numbers')

ggplot2::theme_update(
	panel.background = ggplot2::element_blank(),
	axis.text.x = ggplot2::element_text(colour = "black", face = "plain", size = 10),
	axis.text.y = ggplot2::element_text(colour = "black", face = "plain", size = 10),
	axis.title.x = ggplot2::element_text(colour = "black", face = "plain", size = 12),
	axis.title.y = ggplot2::element_text(colour = "black", face = "plain", angle = 90, size = 12),
	panel.grid.minor = ggplot2::element_blank(),
	panel.grid.major = ggplot2::element_blank(),
	panel.background = ggplot2::element_blank(),
	axis.line = ggplot2::element_line(size = 0.5),
	axis.ticks = ggplot2::element_line(colour = "black", size = 0.5)
)

pop = ggplot2::ggplot(melted, ggplot2::aes(DayInYear, Numbers, group = GooseType)) +
 ggplot2::geom_line(aes(colour = GooseType)) + ggplot2::ylab('Numbers') +
 ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))
return(pop)
}


