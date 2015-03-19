#' Plot goose habitat use
#'
#' Plot the frequency of use of particular habitat types
#' 
#' @param data data.frame or data.table The ALMaSS data file GooseFieldForage.txt
#' @param species character The species to plot. Either 'Barnacle', 'Pinkfoot' or 'Greylag'
#' @return A nice plot
#' @export

PlotGooseHabitatUse = function(data, species = NULL){
	if(is.null(species)) 
	{
		stop('Please provide a species name\nEither Barnacle, Pinkfoot or Greylag\n')
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
	habitatuse =  ggplot(data[species > 0,], aes(factor(Month), fill = VegTypeChr))
	habitatuse = habitatuse + geom_bar(position = 'fill') + theme_bw()
return(habitatuse)
}