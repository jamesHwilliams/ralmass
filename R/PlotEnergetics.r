#' Plot energy output from the ALMaSS Goose Management model
#'
#' Plot the results from the energy probe
#' 
#' The energetics output can be very large, so please use the fread function 
#'  when loading the data. Currently only implemented for pinkfeet due to lack
#'  of field data on barnacle and greylag.
#'  
#' @param SimData data.table The raw output from the file GooseEnergeticsData.txt
#' @param FieldData data.table object cleaned by the function CleanAPIData
#' @param Sample numeric The proportion of the simulated data to include. Value
#' between 0.01 and 1. 
#' @return A nice plot
#' @export
PlotEnergetics = function(SimData, FieldData, Sample = NULL) {
	if(!is.data.table(SimData))
	{
		stop(cat('You appear to have loaded your results file using read.table().\n
			please use the fread function in the package data.table\n'))
	}
	if('Goose Type' %in% names(SimData))
	{
		data.table::setnames(SimData, old = 'Goose Type', new = 'GooseType')
	}

	data.table::setkey(SimData, 'GooseType')
	SimData = SimData[GooseType == 'PFF']

	ys = unique(SimData[,Year])
	ys = ys+2011  # Quick fix to match dates from field data
	ysorigins = as.Date(paste0(ys, '-01-01'))
	for (i in seq_along(ys)) {
		SimData[Year == i, Date:=as.Date(Day, origin = ysorigins[i])]
	}

	SimData[, Season:=c(0, cumsum(diff(Month) > 1))]  # okay
	SimData[, Type:='Sim']

	if(!is.null(Sample))
	{
		nrows = nrow(SimData)
		rows = sample(1:nrows, Sample*nrows)
		SimData = SimData[rows,]
	}

	# FieldData = FieldData[Date > lubridate::dmy('29-08-2012') &
	# Date < lubridate::dmy('31-12-2013')]
	FieldData = FieldData[lubridate::month(FieldData$Date) < 4 | lubridate::month(FieldData$Date) > 9,]
	FieldData[month(Date) < 5, Date:=ymd(paste(2013, month(Date), day(Date), sep = '-'))]
	FieldData[month(Date) > 9, Date:=ymd(paste(2012, month(Date), day(Date), sep = '-'))]
	data.table::setkey(FieldData, Date)  # Otherwise the season breaks
	FieldData[, Season:=c(0, cumsum(diff(Date)/ddays(1) > 100))]
	FieldData[,Season:=Season+100]

	SimData[, Date:=lubridate::ymd(Date)]
	SimData = SimData[GooseType == 'PFF',.(Date, Weight, Type, Season)]
	FieldData[, Type:='Field']
	temp = rbind(SimData, FieldData[, .(Date, Weight, Type, Season)])
	setkey(temp, 'Date')
	temp[, Type:=as.factor(Type)]

	p = ggplot(temp, aes(Date, Weight, color = Type)) +
	geom_point(alpha = 1/50) + 
	geom_smooth(aes(group = Season)) +
	theme_bw() + 
	theme(axis.text=element_text(size=8))
	return(p)
}