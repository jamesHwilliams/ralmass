#' Convert UTM coordinates to ALMaSS map coordinates
#'
#' Convert UTM coordinates into coordinates that can be used in
#' in ALMaSS directly (also works the other way around). Useful when importing
#'  field data into ALMaSS
#' 
#' For Vejlerne there are currently two working maps: VejlerneTest
#' (10000*7000) and VejlerneBigMap (36000*24000).
#' 
#' @param data data.frame The data frame with the coordinates
#' @param long character The name of the column holding the longitude
#' @param lat character The name of the column holding the latitude
#' @param map character The name of the map used in ALMaSS
#' @param subset logical Should the output only contain locations within the
#' ALMaSS map? Ignored when toalmass = FALSE
#' @param toalmass logical Do you want to convert to ALMaSS coordinates. Use FALSE if 
#' if you want to convert ALMaSS map coordinates back to UTM 
#' specified map?
#' @return data.frame A data.frame with the ALMaSS coordinates
#' @export

UtmToALMaSS = function(data, long, lat, map, subset = TRUE, toalmass = TRUE) {
	if(!map %in% c('VejlerneTest', 'VejlerneTest.lsb', 'VejlerneBigMap',
		'VejlerneBigMap.lsb'))
	stop('Invalid map specified. Use either VejlerneTest or VejlerneBigMap')
	stopifnot(is.character(long), is.character(lat))
	if(map == 'VejlerneTest' | map == 'VejlerneTest.lsb')
	{
		x = 494505
		y = 6327722
		dimx = 10000
		dimy = 7000
	}
	if(map == 'VejlerneBigMap' | map == 'VejlerneBigMap.lsb')
	{
		x = 484378
		y = 6335161
		dimx = 36000
		dimy = 24000
	}
	if(toalmass)
	{
		if(is.character(long)) {
			varname = as.name(long)
			r = eval(varname, data)
			data$ALong = floor(data[, r]-x)
		}
		if(is.character(lat)) {
			varname = as.name(lat)
			r = eval(varname, data)
			data$ALat = floor(y - data[, r])	
		}

		if(subset) 
		{
			data = data[which(data$ALong >= 0 & data$ALong <= dimx),]
			data = data[which(data$ALat >= 0 & data$ALat <= dimy),]
		}
		return(data)
	}

	if(!toalmass)
	{
		if(is.character(long)) {
			varname = as.name(long)
			r = eval(varname, data)
			data$ALong = floor(x + data[, r])
		}
		if(is.character(lat)) {
			varname = as.name(lat)
			r = eval(varname, data)
			data$ALat = floor(y - data[, r])	
		}
		if(!is.character(long) & !is.character(lat)) {
			data$ALong = floor(data[,long]+x)
			data$ALat = floor(y-data[,lat])
		}
		return(data)
	}
}

