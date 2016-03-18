#' Classify animal habitat use
#'
#' Classify animal habitat use from what ALMaSS uses into categories
#' matching the ones that are scored in the field
#' 
#' @param data data.table A data.table with the habitat use from ALMaSS
#' @param species character String giving the species (or animal model)
#' @return A data.table with the classifed habitat use.
#' @export
ClassifyHabitatUse = function(data, species = NULL) {
	if(is.null(species))
	{
		stop('Input parameter species missing')
	}
	if(tolower(species) != 'goose') 
	{
		stop('Currently only implemented for geese')
	}
	if(tolower(species) == 'goose')
	{
	# Remove extra white spaces and make combined variable:
		data[, LastSownVeg:=stringr::str_trim(LastSownVeg, side = 'right')]
		data[, VegTypeCombo:=paste(LastSownVeg, VegPhase, sep = '-')]
		data[Pinkfoot > 0, HabitatUsePF:=sapply(VegTypeCombo, ClassifyVegType)]
		data[Greylag > 0, HabitatUseGL:=sapply(VegTypeCombo, ClassifyVegType)]
		data[Barnacle > 0, HabitatUseBN:=sapply(VegTypeCombo, ClassifyVegType)]
		# Fix for a rare bug:
		data[Pinkfoot > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUsePF:='Stubble']
		data[Greylag > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseGL:='Stubble']
		data[Barnacle > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseBN:='Stubble']
		# Fix natural grass types:
		data[Pinkfoot > 0 & VegTypeChr == 'NaturalGrass', HabitatUsePF:='Grass']
		data[Greylag > 0 & VegTypeChr == 'NaturalGrass', HabitatUseGL:='Grass']
		data[Barnacle > 0 & VegTypeChr == 'NaturalGrass', HabitatUseBN:='Grass']
		# Fix undersown spring barley:
		data[Pinkfoot > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			 LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
		data[Greylag > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			 LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
		data[Barnacle > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			 LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		return(data)
	}
}
