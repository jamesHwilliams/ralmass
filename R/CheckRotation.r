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
		for (j in seq_along(crops)) {
			if(crops[j] == 'WinterRye') 
			{
				if(j == length(crops)) 
				{
					if(crops[1] %in% c('CloverGrassGrazed1', 'SeedGrass1')) 
					{
						cat(paste0('Undersown WinterRye. Check last and first crop in ', wd[i], '\n'))
					}
				}
				if(crops[j+1] %in% c('CloverGrassGrazed1', 'SeedGrass1')){
					cat(paste0('Undersown WinterRye on line ', j, ' in ', wd[i], '\n'))
				}
			}
		}
	}
}