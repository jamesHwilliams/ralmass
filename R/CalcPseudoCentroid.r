#' Calculate pseudo centroid for polygons
#'
#' Calculate the mean x and mean y coordinate for a number of polygons
#' Be aware that this is only an approximation. With funny shaped polygons 
#' you might get strange results.
#' 
#' @param polygons data.frame The raw file from the bird database
#' @return The pseudo centroid
#' @export

data = fread('o:/ST_GooseProject/Field data/Fugledata/fugledata_polygoner_20141027.csv')

data$PolygonData = str_replace_all(data[,PolygonData], "\\(", "")
data$PolygonData = str_replace_all(data[,PolygonData], "\\)", "")
poly = data[PolyID == 11]
poly = poly$PolygonData
temp = str_split(data[,PolygonData], ',')[[1]]

temp[which(1:length(temp) %% 2 != 0)]  # Her skal der bare tages et mean - mere eller mindre
temp[which(1:length(temp) %% 2 == 0)]  # Her skal der bare tages et mean - mere eller mindre


 fruits <- c("one apple", "two pears", "three bananas")


CalcPseudoCentroid = function(data) {

}