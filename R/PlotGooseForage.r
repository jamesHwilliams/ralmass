#' Plot forage available to geese
#'
#' Plot the forage available as grazing and grain available to geese in ALMaSS
#' 
#' @param data data.frame or data.table The ALMaSS data file GooseFieldForageData.txt
#' @param species character One of the goose species ('Pinkfoot', 'Barnacle', 'Greylag') defaults to 'Pinkfoot'
#' @param type character What forage should be plotted ('Grass', 'Grain' or 'Combined'). No default.
#' @return A nice plot
#' @export

PlotGooseForage = function(data, species = 'Pinkfoot', type = NULL){
	if(is.null(type) | !is.character(type)) 
	{
		cat('type appears to be missing.\n')
		cat('Please specify type as either:\n')
		cat('Grass, Grain or Combined (character).\n')
	}
	if(!type %in% c('Grass', 'grass', 'Grain', 'grain', 'combined', 'Combined')) 
	{
		cat('Looks like you asked for a type you cant get.\n')
		cat('Please specify type as either:\n')
		cat('Grass, Grain or Combined (character).\n')
	}

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
	col = rgb(0,0,0, alpha = 0.2)
	column = paste('Grass', species, sep = '')

	grass = ggplot2::ggplot(data, ggplot2::aes_string('Day', column, group = 'Polyref')) + ggplot2::geom_line(colour = col) + ggplot2::ylab('Grazing forage available') + ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))
	grain = ggplot2::ggplot(data, ggplot2::aes(Day, Grain, group = Polyref)) + ggplot2::geom_line(colour = col) + ggplot2::ylab('Grain forage available') + ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))

	if(type == 'combined' | type == 'Combined') 
	{
	gridExtra::grid.arrange(grass, grain, nrow = 1, ncol = 2)
	}
	if(type == 'grain' | type == 'Grain') 
	{
		return(grain)
	}
	if(type == 'grass' | type == 'Grass') 
	{
		return(grass)
	}
}
