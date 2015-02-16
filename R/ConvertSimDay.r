#' Supply the calendar date from a sim day
#'
#' Return the calendar date from a simulation day number
#' 
#' @param DayNunber integer A day number
#' @return The calendar date
#' @export
ConvertSimDay = function(DayNumber, Origin = '1990-01-01') {
	origin = '1990-01-01'
    return(lubridate::ymd(origin) + lubridate::days(DayNumber))
}
