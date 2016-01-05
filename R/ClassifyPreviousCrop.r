#' Classify Veg type based on the previous crop with respect to geese forage
#'
#' Classify the veg types from the detailed ones ALMaSS uses into categories
#' matching the ones that are scored in the field.
#' 
#' This function handles the special cases where crop type changes before the 
#' actual farm management takes place in the simulation.
#' The function is designed to be used in combination with 'sapply' 
#' 
#' @param PreviousCrop character A string with the name of the previous crop.
#' @return The vegtype matching the one scored in the field
#' @export
ClassifyPrevCrop = function(PreviousCrop)
{
	switch(EXPR = PreviousCrop,
	# Grass               
		'CloverGrassGrazed1' = 'Grass',
		'OCloverGrassGrazed1' = 'Grass',
		'CloverGrassGrazed2' = 'Grass',
		'OCloverGrassGrazed2' = 'Grass',
		'OBarleyPeaCloverGrass' = 'Grass',
		'OSBarleySilage' = 'Grass',	
		'None' = 'Grass',  # If no preceeding crop, it is a permanent grassy type
	# StubbleUndersown
		'SprBarleyCloverGrass' = 'StubbleUndersown',
		'SpringBarleyCloverGrass' = 'StubbleUndersown',
	# Stubble
		'OSpringBarley' = 'Stubble',
		'MaizeSilage' = 'Stubble',
	# Default
		'SomeFunkyPreceedingCrop')
}