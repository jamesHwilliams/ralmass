#' Plot an Almass weather file
#'
#' Plot an Almass weather file or a subset of it 
#' 
#' If from or to are left blank the entire range of data is plotted
#' 
#' @param file character The full path to the weather file
#' @param from character Date in format year-month-day
#' @param to character Date in format year-month-day
#' @return A nice plot
#' @export
PlotWeather = function(file, from = NULL, to = NULL){
	weather = data.table::fread(file, skip = 1)
	setnames(weather, c('Year', 'Month', 'Day', 'Temperature', 'Wind', 'Rain'))
	weather[,Date:=paste(weather[,Year],weather[,Month],weather[,Day], sep = '-')]
	weather[,Date:=lubridate::ymd(Date)]
	weather[, c('Year', 'Month', 'Day'):=NULL]
	molten = data.table::melt(weather, measure.vars = c('Temperature', 'Wind', 'Rain'))
	if(all(!is.null(from), !is.null(to))) 
	{
	minsubset = as.Date(from)
	maxsubset = as.Date(to)
	if(minsubset < molten[,min(Date)] | maxsubset > molten[,max(Date)])
	{
		return(cat('Subset out of range of the provided weaather file\n'))
	}
	molten = molten[Date %between% c(minsubset, maxsubset)]
	}
	p = ggplot2::ggplot(molten, ggplot2::aes(Date, value)) +
		ggplot2::geom_line() +
		ggplot2::facet_grid(~variable, scales = 'free_y') +
		ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
		ggplot2::theme_bw()
	return(p)
}
