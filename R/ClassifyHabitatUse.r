#' Classify animal habitat use
#'
#' Classify animal habitat use from what ALMaSS uses into categories
#' matching the ones that are scored in the field
#' 
#' @param data data.table A data.table with the habitat use from ALMaSS
#' @param species character String giving the species (or animal model)
#' @return A data.table with the classifed habitat use.
#' @export
ClassifyHabitatUse = function(data, species = 'Goose')
{
	if(tolower(species) != 'goose') 
	{
		stop('Currently only implemented for geese')
	}
	if(tolower(species) == 'goose')
	{
	# Remove extra white spaces and make combined variable:
		data[, VegTypeChr:=str_trim(VegTypeChr, side = 'right')]
		data[, PreviousCrop:=str_trim(PreviousCrop, side = 'right')]
		data[, VegTypeCombo:=paste(VegTypeChr, VegPhase, sep = '-')]

	# Classify habitat use
		data[Pinkfoot > 0, HabitatUsePF:=sapply(VegTypeCombo, FUN = ClassifyVegType)]
		data[Barnacle > 0, HabitatUseBN:=sapply(VegTypeCombo, FUN = ClassifyVegType)]
		data[Greylag > 0, HabitatUseGL:=sapply(VegTypeCombo, FUN = ClassifyVegType)]

		data[Pinkfoot > 0 & VegPhase == 2 & VegHeight < 30, HabitatUsePF:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		data[Barnacle > 0 & VegPhase == 2 & VegHeight < 30, HabitatUseBN:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		data[Greylag > 0 & VegPhase == 2 & VegHeight < 30, HabitatUseGL:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
	# The three species has giving-up densities:
		data[Grain > 98 & !HabitatUseBN %in% c('Maize', 'StubbleUndersown'), HabitatUseBN:='Stubble']
		data[Grain > 241 & !HabitatUseGL %in% c('Maize', 'StubbleUndersown'), HabitatUseGL:='Stubble']
		data[Grain > 134 & !HabitatUsePF %in% c('Maize', 'StubbleUndersown'), HabitatUsePF:='Stubble']
		data[,VegTypeCombo:=NULL]  # No longer needed.
	}
	return(data)
}