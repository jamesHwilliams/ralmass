#' Plot an Almass weather file
#'
#' Plot an Almass weather file or a subset of it 
#' 
#' @param file character The full path to the weather file
#' @param subset numeric Either a year or a sequence of years. If left blank - plot the whole thing.
#' @return A nice plot
#' @export
PlotWeather = function(file, subset = NULL){
	weather = data.table::fread(file, skip = 1)
	setnames(weather, c('Year', 'Month', 'Day', 'Temp', 'Wind', 'Rain'))
	weather[,Julian:=lubridate::ymd(paste(weather[,Year],weather[,Month],weather[,Day], sep = '-'))]
	if(!is.null(subset)) 
	{
	if(min(subset) < min(weather[,Year]) | max(subset) > max(weather[,Year]))
	{
		return(cat('Subset out of range of the provided weather file\n'))
	}
		if(length(subset) > 1) 
		{
			weather = weather[lubridate::year(Julian) >= min(subset) & lubridate::year(Julian) <= max(subset)]
		}
		else
		{
			weather = weather[lubridate::year(Julian) == subset]
		}
	}
	par(mfrow = c(3,1), las = 1, bty = 'l', mar =  c(2, 4, 2, 1) + 0.1)
	plot(weather[,Julian], weather[,Temp], type = 'l', ylab = 'Temperature')
	plot(weather[,Julian], weather[,Rain], type = 'l', ylab = 'Rain')
	plot(weather[,Julian], weather[,Wind], type = 'l', ylab = 'Wind')
}
