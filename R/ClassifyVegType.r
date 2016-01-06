#' Classify Veg type with respect to geese forage
#'
#' Classify the veg types from the detailed ones ALMaSS uses into categories
#' matching the ones that are scored in the field
#' 
#' This function is designed to be used in combination with 'sapply' 
#' 
#' @param VegTypeCombo character A string combined from VegType and VegPhase 
#' separated by '-'
#' @return The vegtype matching the one scored in the field
#' @export
ClassifyVegType = function(VegTypeCombo) {
	switch(EXPR = VegTypeCombo,
	# Grass    
	# These types can only be grass, no matter the vegphase           
		'NaturalGrass-0' = 'Grass',
		'NaturalGrass-2' = 'Grass',
		'CloverGrassGrazed2-0' = 'Grass',
		'CloverGrassGrazed2-2' = 'Grass',
		'OCloverGrassGrazed2-0' = 'Grass',
		'OCloverGrassGrazed2-2' = 'Grass',
		'SeedGrass2-0' = 'Grass',
		'SeedGrass2-2' = 'Grass',
		'SeedGrass2-3' = 'Grass',
		'OSeedGrass2-0' = 'Grass',
		'OSeedGrass2-2' = 'Grass',
		'OSeedGrass2-3' = 'Grass',
		'PermanentGrassGrazed-0' = 'Grass',
		'PermanentGrassGrazed-2' = 'Grass',
		'PermanentGrassGrazed-3' = 'Grass',
		'PermanentGrassLowYield-0' = 'Grass',
		'PermanentGrassLowYield-2' = 'Grass',
		'PermanentGrassLowYield-3' = 'Grass',
		'PermanentSetaside-0' = 'Grass',
		'PermanentSetaside-2' = 'Grass',
		'PermanentSetaside-3' = 'Grass',
		'PermanentGrassTussocky-0' = 'Grass',
		'PermanentGrassTussocky-2' = 'Grass',
		'PermanentGrassTussocky-3' = 'Grass',
		'OPermanentGrassGrazed-0' = 'Grass',
		'OPermanentGrassGrazed-2' = 'Grass',
		'OPermanentGrassGrazed-3' = 'Grass',
	# These types will in some vegphases be something else
		'OCloverGrassGrazed1-2' = 'Grass',
		'OSeedGrass1-3' = 'Grass',
		'CloverGrassGrazed1-2' = 'Grass',
		'CloverGrassGrazed1-3' = 'Grass',
		'OBarleyPeaCloverGrass-1' = 'Grass',
		'OBarleyPeaCloverGrass-3' = 'Grass',
		'SprBarleyCloverGrass-2' = 'Grass',
		'SprBarleyCloverGrass-3' = 'Grass',
		'OSBarleySilage-3' = 'Grass',
		'SpringBarleySilage-3' = 'Grass',
		'OBarleyPeaCloverGrass-2' = 'Grass',
		'OSBarleySilage-2' = 'Grass',
		'SpringBarleySilage-2' = 'Grass',
		'SeedGrass1-3' = 'Grass',
		'OCloverGrassGrazed1-3' = 'Grass',
	# Rape
		'WinterRape-3' = 'Rape',
		'WinterRape-1' = 'Rape',
	# Winter cereal
		'WinterWheat-1' = 'WinterCereal',
		'WinterWheat-2' = 'WinterCereal',
		'WinterWheat-0' = 'WinterCereal',
		'SpringBarley-1' = 'WinterCereal',
		'SpringBarley-2' = 'WinterCereal',
		'OSpringBarley-1' = 'WinterCereal',
		'OSBarleySilage-1' = 'WinterCereal',
		'WinterBarley-0' = 'WinterCereal',
		'WinterBarley-1' = 'WinterCereal',
		'WinterBarley-2' = 'WinterCereal',
		'OWinterRye-0' = 'WinterCereal',
		'OWinterRye-1' = 'WinterCereal',
		'WinterRye-0' = 'WinterCereal',
		'WinterRye-1' = 'WinterCereal',
		'OWinterWheatUndersown-1' = 'WinterCereal',
		'OOats-1' = 'WinterCereal',
		'OOats-2' = 'WinterCereal',
		'Oats-1' = 'WinterCereal',
		'Oats-2' = 'WinterCereal',
		'Triticale-1' = 'WinterCereal',
	# Stubble
		'WinterBarley-3' = 'Stubble',
		'OSpringBarley-3' = 'Stubble',
		'WinterRye-3' = 'Stubble',
		'WinterWheat-3' = 'Stubble',
		'SpringBarley-3' = 'Stubble',
		'Oats-3' = 'Stubble',
		'OOats-3' = 'Stubble',
		'OTriticale-3' = 'Stubble',
		'Triticale-3' = 'Stubble',
		'OWinterRye-3' = 'Stubble',
		'OFieldPeas-3' = 'Stubble',
		'OCarrots-3' = 'Stubble',
	# Undersown stubble
		'OWinterWheatUndersown-3' = 'StubbleUndersown',
	# Maize stubble
		'MaizeSilage-2' = 'Maize',
		'MaizeSilage-3' = 'Maize',
		'OMaizeSilage-2' = 'Maize',
		'OMaizeSilage-3' = 'Maize',
	# Default
		'SomethingFunky')
}