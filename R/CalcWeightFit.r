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
	ys = unique(SimData[,Year])
	ys = ys+2011  # Quick fix to match dates from field data
	ysorigins = as.Date(paste0(ys, '-01-01'))
# setkey(data, 'Goose Type')
	for (i in seq_along(ys)) {
		SimData[Year == i, Date:=as.Date(Day, origin = ysorigins[i])]
	}
	SimData[, Season:=c(0, cumsum(diff(Month) > 1))]  # okay
# SimData[, Type:='Sim']
	SimData[, SWeight:=Weight/max(Weight)]

	simmean = SimData[, mean(SWeight), by = .(lubridate::week(Date), Season)]
	setnames(simmean, c('Week', 'Season', 'AvgWeightSim'))
	setkey(simmean, Week)

	FieldData = FieldData[Date > ymd('2010-08-01') & Date < ymd('2015-05-31'),]
	FieldData[, SWeight:=Weight/max(Weight)]
	FieldDatamean = FieldData[, mean(SWeight), by = lubridate::week(Date)]
	setnames(FieldDatamean, c('Week', 'AvgWeightField'))
	setkey(FieldDatamean, Week)

# Calculate least squares
	seasons = unique(SimData[, Season])
	lsfits = rep(NA, length(seasons))
	for (i in seq_along(seasons)) {
		full = merge(FieldDatamean, simmean[Season == seasons[i], .(Week, AvgWeightSim)])
		lsfits[i] = with(full, 1-sum((AvgWeightSim-AvgWeightField)^2))
	}
	return(lsfits)
}

