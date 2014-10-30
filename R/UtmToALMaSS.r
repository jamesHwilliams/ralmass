#' Convert UTM coordinates to ALMaSS map coordinates
#'
#' Convert UTM coordinates into coordinates that can be used in
#' in ALMaSS directly. Useful when importing field data into ALMaSS
#' 
#' For Vejlerne there are currently two working maps: VejlerneTest
#' (10000*7000) and VejlerneBigMap (36000*24000).
#' 
#' @param data data.frame The data frame with the coordinates
#' @param long character The name of the column holding the longitude
#' @param lat character The name of the column holding the latitude
#' @param map character The name of the map used in ALMaSS
#' @param map logical Should the output only contain locations within the 
#' specified map?
#' @return data.frame A data.frame with the ALMaSS coordinates
#' @export

UtmToALMaSS = function(data, long, lat, map, subset = TRUE) {
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

data$ALong = floor(data[,long]-x)
data$ALat = floor(y-data[,lat])

if(subset) 
{
	data = data[which(data$ALong >= 0 && data$ALong <= dimx),]
	data = data[which(data$ALat >= 0 && data$ALat <= dimy),]
}
return(data)
}