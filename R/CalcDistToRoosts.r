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
CalcDistToRoosts = function(roost = NULL, fields = NULL, fieldobs = NULL, polyref = NULL, species = NULL) {
	if(any(is.null(roost),is.null(fields),is.null(fieldobs),is.null(species))){
		stop('Required input argument missing')
	}
	if(!fieldobs && is.null(polyref)) {
		stop('Provide polyref file when fieldobs = FALSE')
	}
	setnames(roost, c('Type', 'CentroidX', 'CentroidY'))
	if(fieldobs) {
			setnames(fields, old = c('ALong', 'ALat'), new = c('CentroidX', 'CentroidY'))
	}
	roost[, Type:=sapply(Type, FUN = SwapType)]
	TheList = vector('list', length(species))
	for (i in seq_along(species)) {
		Roost = roost[Type == species[i],]  # Get the roost for the species
		if(fieldobs) {
			Fields = fields[Species == species[i], .(CentroidX, CentroidY)]
			DT = fields[,.(PolyID)]
		}
		if(!fieldobs) {
			spobs = eval(as.name(species[i]), fields)
			ForagePolys = unique(fields[spobs != 0,Polyref])
			Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
			DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		}
		for(j in 1:nrow(Roost))
		{
			TheDistances = dist(rbind(Roost[j,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', j)
			DT[,newcolname:=TheDistances, with = FALSE]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT), with = FALSE], FUN = min, MARGIN = 1)]
		DT[,GooseType:=species[i]]
		nullcols = grep('Roost', names(DT))
		DT[, nullcols:=NULL]
		TheList[[i]] = DT
	}
	return(data.table::rbindlist(TheList))
}

# Helper function
SwapType = function(Type) {
	switch(EXPR = as.character(Type),
		'0' = 'Pinkfoot',
		'1' = 'Barnacle',
		'2' = 'Greylag',
		'Undefined species')
}
