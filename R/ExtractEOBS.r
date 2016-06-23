#' Extract values from E-OBS netCDF files
#'
#' Extract values based on a spatial object from the E-OBS data.
#' Useful when making weather files for ALMaSS. Often a point in the center
#' of the landscape can be used to extract values. For the bigger landscapes
#' a mean for the landscapes might make more sense. The function can handle
#' that too. 
#' 
#' If the polygon does not overlap any cell centroids in the E-OBS raster or 
#' if the point does not fall within any of the cells, the 'bilinear' method is
#' used. This will interpolate the value from the nearest four cells from
#' either the centroid (if spobject is a SpatialPolygon) or the point (if 
#' spobject is a SpatialPoint)
#' 
#' @param eobs character Vector of full paths to the eobs nc files
#' @param spobject Spatial* The geometry to extract with (point of polygon)
#' @param metric character The metric to use when spoject is a polygon.  Will
#' normally be 'mean', 'min' or 'max'
#' @export
ExtractEOBS = function(eobs = NULL, spobject = NULL, metric = 'mean') {
	if(any(is.null(eobs),is.null(spobject))) {
		stop('Input missing')
	}
	eobsnames = basename(eobs)
	vars = sapply(eobsnames, FUN = strsplit, split = '_') 
	vars = sapply(vars, '[',1)
	if(length(vars) > 1){
		VarList = vector('list', length(vars))
	}
	for (i in seq_along(vars)) {
		b = raster::brick(eobs[i], varname = vars[i], level = 3)
		if(!identical(raster::projection(b), raster::projection(spobject))) {
			stop('Projections not idential')
		}
		if("SpatialPoints" == class(spobject)) {
			vals = raster::extract(b, spobject, df = FALSE)
			if(all(is.na(vals))) {
				vals = raster::extract(b, spobject, fun = metric,
					df = FALSE, na.rm = TRUE, method = 'bilinear')
			}
			vals = data.table::as.data.table(t(vals))
		}
		if("SpatialPolygons" == class(spobject)) {
			vals = raster::extract(b, spobject, fun = metric,
				df = FALSE, na.rm = TRUE)
			if(all(is.na(vals))) {
				vals = raster::extract(b, coordinates(spobject), fun = metric,
					df = FALSE, na.rm = TRUE, method = 'bilinear')
			}
			vals = data.table::as.data.table(t(vals))
		}
		vals[, Date:=raster::getZ(b)]
		data.table::setnames(vals, old = 'V1', new = GetVarName(vars[i]))
		data.table::setkey(vals, Date)
		if(length(vars) > 1) {
			VarList[[i]] = vals
		}
	}
	if(length(vars) > 1) {
		vals = do.call(merge, VarList)
	}
	vals[, Date:=as.Date(Date)]
	vals[, Month:=data.table::month(Date)]
	vals[, Day:=data.table::mday(Date)]
	vals[, Year:=data.table::year(Date)]
	vals[, Date:=NULL]
	vals[, Wind:=-1]  # Needed in the ALMaSS weather files
	return(vals)
}

GetVarName = function(var) {
	switch(var,
		'tg' = 'MeanTemperature',
		'rr' = 'Precipitation',
		'tx' = 'MaxTemperature',
		'tn' = 'MinTemperature',
		'foobar'
		)
}
