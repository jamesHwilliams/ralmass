#' Plot forage available to geese
#'
#' Plot the forage available as grazing and grain available to geese in ALMaSS
#' 
#' @param data data.frame or data.table The ALMaSS data file GooseFieldForageData.txt
#' @param species character One of the goose species ('Pinkfoot', 'Barnacle', 'Greylag') defaults to 'Pinkfoot'
#' @return A nice plot
#' @export

PlotGooseForage = function(data, species = 'Pinkfoot'){
	theme_update(
		panel.background = element_blank(),
		axis.text.x = element_text(colour = "black", face = "plain", size = 10),
		axis.text.y = element_text(colour = "black", face = "plain", size = 10),
		axis.title.x = element_text(colour = "black",	face = "plain", size = 12),
		axis.title.y = element_text(colour = "black", face = "plain", angle = 90, size = 12),
		panel.grid.minor = element_blank(),
		panel.grid.major = element_blank(),
		panel.background = element_blank(),
		axis.line = element_line(size = 0.5),
		axis.ticks = element_line(colour = "black", size = 0.5)
		)
	col = rgb(0,0,0, alpha = 0.2)
	column = paste('Grass', species, sep = '')
	grass = ggplot(data, aes_string('Day', column, group = 'Polyref')) + geom_line(colour = col) + ylab('Grazing forage available')
	grain = ggplot(data, aes(Day, Grain, group = Polyref)) + geom_line(colour = col) + ylab('Grain forage available')
	grid.arrange(grass, grain, nrow = 1, ncol = 2)
}
