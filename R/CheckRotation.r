#' Check crop rotations
#'
#' Do a check of the crop rotations. There are certain crops that never should
#' be grown in to consecutive years for example. There is also a check to  
#' ensure that crops that are undersown other crops are valid combinations 
#' (some crops are never grown with an undersown crop)
#' The function will print the results of the check to the console. 
#' 
#' @param path character The path to the rotations to check. This can be the
#' run directory.
#' @export
CheckRotation = function(path = NULL) {
	if(is.null(path)) {
		stop('Path missing')
	}
	wd = dir(path)
	wd = wd[grep('.rot', wd)]
	for (i in seq_along(wd)) {
		rot = fread(paste0(path, wd[i]), skip = 4, header = FALSE)
		crops = rot[,V1]
		locs = diff(which(crops == 'WinterRye'))
		if(any(locs == 1)) {
			cat(paste0('WinterRye following WinterRye in ', wd[i], '\n'))
		}
		locs = diff(which(crops == 'WinterRape'))
		if(any(locs == 1)) {
			cat(paste0('WinterRape following WinterRape in ', wd[i], '\n'))
		}
		locs = diff(which(crops == 'Potatoes'))
		if(any(locs == 1)) {
			cat(paste0('Potatoes following Potatoes in ', wd[i], '\n'))
		}
		locs = diff(which(crops == 'PotatoesIndustry'))
		if(any(locs == 1)) {
			cat(paste0('PotatoesIndustry following PotatoesIndustry in ', wd[i], '\n'))
		}
		locs = diff(which(crops == 'CloverGrassGrazed1'))
		if(any(locs == 1)) {
			cat(paste0('CloverGrassGrazed1 following CloverGrassGrazed1 in ', wd[i], '\n'))
		}
		for (j in seq_along(crops)) {
			if(crops[j] %in% c('WinterRye', 'WinterWheat')) 
			{
				if(j == length(crops)) 
				{
					if(crops[1] %in% c('CloverGrassGrazed1', 'SeedGrass1', 'FodderGrass')) 
					{
						cat(paste0('Undersown WinterRye or WinterWheat. Check last and first crop in ', wd[i], '\n'))
					}
				}
				if(j != length(crops)) 
				{
					if(crops[j+1] %in% c('CloverGrassGrazed1', 'SeedGrass1', 'FodderGrass')){
						cat(paste0('Undersown WinterRye or WinterWheat on line ', j+4, ' in ', wd[i], '\n'))
					}
				}
			}
			if(crops[j] == 'SpringBarley') 
			{
				if(j != length(crops)) {
					if(crops[j+1] == 'CloverGrassGrazed1') 
					{
						cat(paste0('CloverGrassGrazed1 following SpringBarley on line ', j+4, ' in ', wd[i], '\n'))
					}
				}
				if(j == length(crops)) {
					if(crops[1] == 'CloverGrassGrazed1') 
					{
						cat(paste0('CloverGrassGrazed1 following SpringBarley. Check last and first crop in in ', wd[i], '\n'))
					}
				}
			}
		}
	}
	cat('Checking done \n')
}