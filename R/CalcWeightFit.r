#' Calculate the fit of the simulated weights to the weights observed
#'
#' Calculate a least squares fit between field weights and simulated weights. 
#' The weights are scaled to go from 0 - 1 on the indivual observations.
#' A period of time must be chosen to calculate means within.
#' 
#' @param SimData data.table The file GooseEnergeticsData.txt
#' @param FieldData data.table The observed weights
#' @export
CalcWeightFit = function(SimData, FieldData) {
	SimData[, Date:=as.Date(Day, origin = '2012-01-01')]
	SimData[, Season:=c(0, cumsum(diff(data.table::month(Date)) > 1))]  # okay

	simmean = SimData[, mean(Weight), by = .(data.table::week(Date), Season)]
	data.table::setnames(simmean, c('Week', 'Season', 'AvgWeightSim'))
	data.table::setkey(simmean, Week)

	FieldData = FieldData[Date > lubridate::ymd('2010-08-01') & 
							Date < lubridate::ymd('2015-05-31'),]
	FieldDatamean = FieldData[, mean(Weight), by = data.table::week(Date)]
	data.table::setnames(FieldDatamean, c('Week', 'AvgWeightField'))
	data.table::setkey(FieldDatamean, Week)

# Calculate least squares
	seasons = unique(SimData[, Season])
	lsfits = rep(NA, length(seasons))
	for (i in seq_along(seasons)) {
		full = merge(FieldDatamean, simmean[Season == seasons[i], .(Week, AvgWeightSim)])
		themin = min(full[,.(AvgWeightSim, AvgWeightField)])
		themax = max(full[,.(AvgWeightSim, AvgWeightField)])
		denum = themax-themin
		full[, AvgWeightSim:=(AvgWeightSim-themin)/denum, by = Week]
		full[, AvgWeightField:=(AvgWeightField-themin)/denum, by = Week]
		lsfits[i] = with(full, 1-sum((AvgWeightSim-AvgWeightField)^2))
	}
	names(lsfits) = paste0('Season', seasons)
	return(lsfits)
}
