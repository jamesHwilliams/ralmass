#' Supply the calendar date from a sim day
#'
#' Return the calendar date from a simulation day number
#' 
#' @param DayNumber integer A day number
#' @param Origin Date Origin for Julian day calculation (YYYY-MM-DD)
#' @return The calendar date
#' @export
ConvertSimDay = function(DayNumber, Origin = '1990-01-01') {
	origin = '1990-01-01'
    return(lubridate::ymd(origin) + lubridate::days(DayNumber))
}
