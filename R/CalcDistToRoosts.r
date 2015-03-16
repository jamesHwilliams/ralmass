#' Calculate the distance from fields to goose roosts
#'
#' Calculate the distance from fields to goose roosts (in this case)
#' Using the centroids from the polyref file distances can be calculated 
#' outside of ALMaSS. 
#' 
#' @param roost data.table The object with the GooseRoosts.txt file
#' @param fields data.table The object with the GooseFieldForageData.txt file
#' @param polyref data.table The object with the polyref file
#' @param species character The species for which the distances should be 
#' calculated for. Either 'Barnacle', 'Pinkfoot' or 'Greylag'
#' @return list A list holding a data.table with distances for each roost
#' @export

CalcDistToForageLocation = function(roost, fields, polyref, species)
{
	setnames(roost, c('Type', 'CentroidX', 'CentroidY'))
	if(tolower(species) == 'pinkfoot') 
	{
		ForagePolys = unique(fields[Barnacle != 0,Polyref])
		roost = roost[Type == 0,]
	}
	if(tolower(species) == 'barnacle') 
	{
		ForagePolys = unique(fields[Barnacle != 0,Polyref])
		roost = roost[Type == 1,]
	}
	if(tolower(species) == 'greylag') 
	{
		ForagePolys = unique(fields[Barnacle != 0,Polyref])
		roost = roost[Type == 2,]
	}
	FieldsInUse = polyref[PolyRefNum %in% ForagePolys, c('CentroidX', 'CentroidY'), with = FALSE]
	TheList = list()
	for(i in 1:nrow(roost))
	{
		TheDistances = dist(rbind(roost[1,c('CentroidX', 'CentroidY'), with = FALSE], FieldsInUse))[1:nrow(FieldsInUse)+1]
		TheList[[i]] = data.table('DistToRoost' = TheDistances)
	}
	return(TheList)
}
