#' Classify animal habitat use
#'
#' Classify animal habitat use from what ALMaSS uses into categories
#' matching the ones that are scored in the field
#' 
#' @param data data.table A data.table with the habitat use from ALMaSS
#' @param species character String giving the species (or animal model)
#' @return A data.table with the classifed habitat use.
#' @export
ClassifyHabitatUse = function(data, species = NULL)
{
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
		# data = forage
		data = data[Geese > 0]
		keycols = c('Year', 'Month', 'Day', 'Polyref')
		data.table::setkeyv(data, keycols)
	# Remove extra white spaces and make combined variable:
		data[, VegTypeChr:=stringr::str_trim(VegTypeChr, side = 'right')]
		data[, PreviousCrop:=stringr::str_trim(PreviousCrop, side = 'right')]
		data[, VegTypeCombo:=paste(VegTypeChr, VegPhase, sep = '-')]
	# Determine sowing time:
		data[, SowingTime:=sapply(VegTypeChr, FUN = SowTime)]	
	# Add columns
		# data[, c('HabitatUsePF', 'HabitatUseBN', 'HabitatUseGL'):=rep('foo', nrow(data))]
		# Pinkfoot ----
		pf = data[Pinkfoot > 0]    
		pf[, HabitatUsePF:=rep('foo', nrow(pf))]
		# nrow(pf)  # 23263
		pf[, HabitatUsePF:=sapply(VegTypeCombo, ClassifyVegTypeGrass)]
		grass = pf[HabitatUsePF == 'Grass']
		# nrow(grass)
		pf = pf[HabitatUsePF != 'Grass']
		# nrow(pf)
		pf[SowingTime == 'spring' & VegPhase == 1, HabitatUsePF:='WinterCereal']
		wintercereal = pf[HabitatUsePF == 'WinterCereal']
		# nrow(wintercereal)	
		pf = pf[HabitatUsePF != 'WinterCereal']
		# nrow(pf)
		pf[VegPhase == 0 & SowingTime == 'spring', HabitatUsePF:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		prevcrop = pf[HabitatUsePF != 'NotGrass']
		# nrow(prevcrop)
		pf = pf[HabitatUsePF == 'NotGrass']
		# nrow(pf)
		pf[, HabitatUsePF:=sapply(VegTypeCombo, FUN = ClassifyVegType)]
		# pf[HabitatUsePF == 'SomethingFunky']
		pf[HabitatUsePF == 'SomethingFunky', HabitatUsePF:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		# pf[HabitatUsePF == 'SomeFunkyPreceedingCrop']

		pf[VegPhase == 2 & VegHeight < 30, HabitatUsePF:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		pf[VegPhase == 0 & Grain != 0, HabitatUsePF:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		pf[Grain > 134 & !HabitatUsePF %in% c('Maize', 'StubbleUndersown'), HabitatUsePF:='Stubble']

		PF = rbind(grass, wintercereal, prevcrop, pf)
		# nrow(PF)  # Okay
		PF = PF[, .(Year, Month, Day, Polyref, HabitatUsePF)]
		data.table::setkeyv(PF, keycols)

		data = data.table::merge(data, PF, all.x = TRUE)
		# Pinkfoot end ----

		# Greylag ----
		gl = data[Greylag > 0]    
		gl[, HabitatUseGL:=rep('foo', nrow(gl))]
		# nrow(gl)  
		gl[, HabitatUseGL:=sapply(VegTypeCombo, ClassifyVegTypeGrass)]
		grass = gl[HabitatUseGL == 'Grass']
		# nrow(grass) 
		gl = gl[HabitatUseGL != 'Grass']
		# nrow(gl)  
		gl[SowingTime == 'spring' & VegPhase == 1, HabitatUseGL:='WinterCereal']
		wintercereal = gl[HabitatUseGL == 'WinterCereal']
		# nrow(wintercereal)	
		gl = gl[HabitatUseGL != 'WinterCereal']
		# nrow(gl)
		gl[VegPhase == 0 & SowingTime == 'spring', HabitatUseGL:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		prevcrop = gl[HabitatUseGL != 'NotGrass']
		# nrow(prevcrop)  
		gl = gl[HabitatUseGL == 'NotGrass']
		# nrow(gl)  
		gl[, HabitatUseGL:=sapply(VegTypeCombo, FUN = ClassifyVegType)]
		# gl[HabitatUseGL == 'SomethingFunky']
		gl[HabitatUseGL == 'SomethingFunky', HabitatUseGL:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		# gl[HabitatUseGL == 'SomeFunkyPreceedingCrop']

		gl[VegPhase == 2 & VegHeight < 30, HabitatUseGL:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		gl[VegPhase == 0 & Grain != 0, HabitatUseGL:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		gl[Grain > 241 & !HabitatUseGL %in% c('Maize', 'StubbleUndersown'), HabitatUseGL:='Stubble']

		GL = rbind(grass, wintercereal, prevcrop, GL)
		# nrow(GL)  # Okay
		GL = GL[, .(Year, Month, Day, Polyref, HabitatUseGL)]
		data.table::setkeyv(GL, keycols)

		data = data.table::merge(data, GL, all.x = TRUE)
		# Greylag end ----

		# Barnacle ----
		bn = data[Barnacle > 0]    
		bn[, HabitatUseBN:=rep('foo', nrow(bn))]
		# nrow(bn)  
		bn[, HabitatUseBN:=sapply(VegTypeCombo, ClassifyVegTypeGrass)]
		grass = bn[HabitatUseBN == 'Grass']
		# nrow(grass) 
		bn = bn[HabitatUseBN != 'Grass']
		# nrow(bn)  
		bn[SowingTime == 'spring' & VegPhase == 1, HabitatUseBN:='WinterCereal']
		wintercereal = bn[HabitatUseBN == 'WinterCereal']
		# nrow(wintercereal)	
		bn = bn[HabitatUseBN != 'WinterCereal']
		# nrow(bn)
		bn[VegPhase == 0 & SowingTime == 'spring', HabitatUseBN:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		prevcrop = bn[HabitatUseBN != 'NotGrass']
		# nrow(prevcrop)  
		bn = bn[HabitatUseBN == 'NotGrass']
		# nrow(bn)  
		bn[, HabitatUseBN:=sapply(VegTypeCombo, FUN = ClassifyVegType)]
		# bn[HabitatUseBN == 'SomethingFunky']
		bn[HabitatUseBN == 'SomethingFunky', HabitatUseBN:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		# bn[HabitatUseBN == 'SomeFunkyPreceedingCrop']

		bn[VegPhase == 2 & VegHeight < 30, HabitatUseBN:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		bn[VegPhase == 0 & Grain != 0, HabitatUseBN:=sapply(PreviousCrop, FUN = ClassifyPrevCrop)]
		bn[Grain > 98 & !HabitatUseBN %in% c('Maize', 'StubbleUndersown'), HabitatUseBN:='Stubble']

		BN = rbind(grass, wintercereal, prevcrop, bn)
		# nrow(BN)  # Okay
		BN = BN[, .(Year, Month, Day, Polyref, HabitatUseBN)]
		data.table::setkeyv(BN, keycols)

		data = data.table::merge(data, BN, all.x = TRUE)
		# Barnacle end ----
		data[,VegTypeCombo:=NULL]  # No longer needed.
	}
	return(data)
}

ClassifyVegTypeGrass = function(VegTypeCombo) {
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
	# Default
		'NotGrass')
}
SowTime = function(VegType) {
	switch(EXPR = VegType,
		# Spring
		'BroadBeans' = 'spring',
		'Carrots' = 'spring',
		'FieldPeas' = 'spring',
		'FieldPeasSilage' = 'spring',
		'FieldPeasStrigling' = 'spring',
		'FodderBeet' = 'spring',
		'Maize' = 'spring',
		'MaizeSilage' = 'spring',
		'MaizeStrigling' = 'spring',
		'OBarleyPeaCloverGrass' = 'spring',
		'OCarrots' = 'spring',
		'OFieldPeas' = 'spring',
		'OFieldPeasSilage' = 'spring',
		'OFodderBeet' = 'spring',
		'OMaizeSilage' = 'spring',
		'OOats' = 'spring',
		'OPotatoes' = 'spring',
		'OSBarleySilage' = 'spring',
		'OSpringBarley' = 'spring',
		'OSpringBarleyClover' = 'spring',
		'OSpringBarleyExt' = 'spring',
		'OSpringBarleyGrass' = 'spring',
		'OSpringBarleyPigs' = 'spring',
		'Oats' = 'spring',
		'Potatoes' = 'spring',
		'PotatoesIndustry' = 'spring',
		'SpringBarley' = 'spring',
		'SpringBarleyCloverGrass' = 'spring',
		'SpringBarleyCloverGrassStrigling' = 'spring',
		'SpringBarleyGrass' = 'spring',
		'SpringBarleyPTreatment' = 'spring',
		'SpringBarleyPeaCloverGrassStrigling' = 'spring',
		'SpringBarleySKManagement' = 'spring',
		'SpringBarleySeed' = 'spring',
		'SpringBarleySilage' = 'spring',
		'SpringBarleySpr' = 'spring',
		'SpringBarleyStrigling' = 'spring',
		'SpringBarleyStriglingCulm' = 'spring',
		'SpringBarleyStriglingSingle' = 'spring',
		'SpringRape' = 'spring',
		'SpringWheat' = 'spring',
		'SugarBeet' = 'spring',
		# Autumn
		'OTriticale' = 'autumn',
		'OWinterBarley' = 'autumn',
		'OWinterBarleyExt' = 'autumn',
		'OWinterRape' = 'autumn',
		'OWinterRye' = 'autumn',
		'OWinterWheatUndersown' = 'autumn',
		'OWinterWheatUndersownExt' = 'autumn',
		'Triticale' = 'autumn',
		'WinterBarley' = 'autumn',
		'WinterBarleyStrigling' = 'autumn',
		'WinterRape' = 'autumn',
		'WinterRapeStrigling' = 'autumn',
		'WinterRye' = 'autumn',
		'WinterRyeStrigling' = 'autumn',
		'WinterWheat' = 'autumn',
		'WinterWheatShort' = 'autumn',
		'WinterWheatStrigling' = 'autumn',
		'WinterWheatStriglingCulm' = 'autumn',
		'WinterWheatStriglingSingle' = 'autumn',
		# Default
		'NA')
	}