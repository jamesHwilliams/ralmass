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
#' @return data.table A data.table with the polyrefnumber, distances for
#' each roost and a column with the shortest distance.
#' @export

CalcDistToRoosts = function(roost, fields, polyref, species)
{
	setnames(roost, c('Type', 'CentroidX', 'CentroidY'))
	if(tolower(species) == 'all') 
	{
		TheList = vector('list', 3)
	# Pinkfoot
		ForagePolys = unique(fields[Pinkfoot != 0,Polyref])
		Roost = roost[Type == 0,]
		Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
		DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		for(i in 1:nrow(Roost))
		{
			TheDistances = dist(rbind(Roost[i,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', i)
			DT[,newcolname:=TheDistances, with = FALSE]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT), with = FALSE], FUN = min, MARGIN = 1)]
		DT[,GooseType:='Pinkfoot']
		TheList[[1]] = DT[, .(PolyRefNum, Shortest, GooseType)]
	# Barnacle
		ForagePolys = unique(fields[Barnacle != 0,Polyref])
		Roost = roost[Type == 1,]
		Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
		DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		for(i in 1:nrow(Roost))
		{
			TheDistances = dist(rbind(Roost[i,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', i)
			DT[,newcolname:=TheDistances, with = FALSE]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT), with = FALSE], FUN = min, MARGIN = 1)]
		DT[,GooseType:='Barnacle']
		TheList[[2]] = DT[, .(PolyRefNum, Shortest, GooseType)]
	# Greylag
		ForagePolys = unique(fields[Greylag != 0,Polyref])
		Roost = roost[Type == 2,]
		Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
		DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		for(i in 1:nrow(Roost))
		{
			TheDistances = dist(rbind(Roost[i,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', i)
			DT[,newcolname:=TheDistances, with = FALSE]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT), with = FALSE], FUN = min, MARGIN = 1)]
		DT[,GooseType:='Greylag']
		TheList[[3]] = DT[, .(PolyRefNum, Shortest, GooseType)]
		return(data.table::rbindlist(TheList))
	}
	if(tolower(species) != 'all') 
	{
		
		if(tolower(species) == 'pinkfoot') 
		{
			ForagePolys = unique(fields[Pinkfoot != 0,Polyref])
			roost = roost[Type == 0,]
		}
		if(tolower(species) == 'barnacle') 
		{
			ForagePolys = unique(fields[Barnacle != 0,Polyref])
			roost = roost[Type == 1,]
		}
		if(tolower(species) == 'greylag') 
		{
			ForagePolys = unique(fields[Greylag != 0,Polyref])
			roost = roost[Type == 2,]
		}
		Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
		DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		for(i in 1:nrow(roost))
		{
			TheDistances = dist(rbind(roost[i,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', i)
			DT[,newcolname:=TheDistances, with = FALSE]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT), with = FALSE], FUN = min, MARGIN = 1)]
		return(DT)
	}
}
