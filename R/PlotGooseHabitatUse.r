#' Plot goose habitat use
#'
#' Plot the frequency of use of particular habitat types
#' 
#' @param data data.frame or data.table The ALMaSS data file GooseFieldForage.txt
#' @param species character The species to plot. Either 'Barnacle', 'Pinkfoot' or 'Greylag'
#' @param variable character What variable are we plotting? Either 'VegTypeChr'
#' or 'HabitatUse'.
#' @return A nice plot
#' @export

PlotGooseHabitatUse = function(data, species = NULL, variable = NULL) {
	if(is.null(species)) 
	{
		stop('Please provide a species name\nEither Barnacle, Pinkfoot or Greylag\n')
	}
	if(is.null(variable)) 
	{
		stop('Please provide a variable name\nEither VegTypeChr or HabitatUse\n')
	}
	species = tolower(species)
	if(species == 'barnacle') 
	{
		data = data[Barnacle > 0,]
	}
	if(species == 'pinkfoot') 
	{
		data = data[Pinkfoot > 0,]
	}
	if(species == 'greylag') 
	{
		data = data[Greylag > 0,]
	}
	data[,Month:=as.factor(Month)]
	habitatuse =  ggplot(data[species > 0,], aes_string('Month', fill = variable))
	habitatuse = habitatuse + geom_bar(position = 'fill') + theme_bw()
return(habitatuse)
}