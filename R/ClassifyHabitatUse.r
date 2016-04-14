#' Classify animal habitat use
#'
#' Classify animal habitat use from what ALMaSS uses into categories
#' matching the ones that are scored in the field
#' 
#' @param data data.table A data.table with the habitat use from ALMaSS
#' @param species character String giving the species (or animal model)
#' @param timed logical Should we use timed counts (TRUE, default) or daily
#' maximum (FALSE)?
#' @return A data.table with the classifed habitat use.
#' @export
ClassifyHabitatUse = function(data, species = NULL, timed = TRUE) {
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
		data[,c('HabitatUsePF', 'HabitatUseGL', 'HabitatUseBN'):='foo']
		if(!timed)
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
		# Fix undersown spring barley - CGG1:
			data[Pinkfoot > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[Greylag > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[Barnacle > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
			data[Pinkfoot > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[Greylag > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[Barnacle > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		# SG1
			data[Pinkfoot > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[Greylag > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[Barnacle > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseBN:='Stubble']
			data[Pinkfoot > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[Greylag > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[Barnacle > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseBN:='Stubble']
		# Spring barley that is not undersown
			data[Pinkfoot > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[Greylag > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[Barnacle > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		}
		if(timed)
		{
		# Remove extra white spaces and make combined variable:
			data[, LastSownVeg:=stringr::str_trim(LastSownVeg, side = 'right')]
			data[, VegTypeCombo:=paste(LastSownVeg, VegPhase, sep = '-')]
			data[PinkfootTimed > 0, HabitatUsePF:=sapply(VegTypeCombo, ClassifyVegType)]
			data[GreylagTimed > 0, HabitatUseGL:=sapply(VegTypeCombo, ClassifyVegType)]
			data[BarnacleTimed > 0, HabitatUseBN:=sapply(VegTypeCombo, ClassifyVegType)]
		# Fix for a rare bug:
			data[PinkfootTimed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseBN:='Stubble']
		# Fix natural grass types:
			data[PinkfootTimed > 0 & VegTypeChr == 'NaturalGrass', HabitatUsePF:='Grass']
			data[GreylagTimed > 0 & VegTypeChr == 'NaturalGrass', HabitatUseGL:='Grass']
			data[BarnacleTimed > 0 & VegTypeChr == 'NaturalGrass', HabitatUseBN:='Grass']
		# Fix undersown spring barley - CGG1:
			data[PinkfootTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
			data[PinkfootTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		# SG1
			data[PinkfootTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseBN:='Stubble']
			data[PinkfootTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & PreviousCrop == 'SprBarleyCloverGrass' &
			LastSownVeg == 'SeedGrass1', HabitatUseBN:='Stubble']
		# Spring barley that is not undersown
			data[PinkfootTimed > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[GreylagTimed > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[BarnacleTimed > 0 & PreviousCrop == 'SpringBarley' &
			LastSownVeg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		}
	data[HabitatUseBN == 'foo', HabitatUseBN:=NA]
	data[HabitatUsePF == 'foo', HabitatUsePF:=NA]
	data[HabitatUseGL == 'foo', HabitatUseGL:=NA]
	return(data)
	}
}
