#' Plot energy output from the ALMaSS Goose Management model
#'
#' Plot the results from the energy probe
#' 
#' The energetics output can be very large, so please use the fread function 
#'  when loading the data. Currently only implemented for pinkfeet due to lack
#'  of field data on barnacle and greylag.
#'  
#' @param data data.table The raw output from the file GooseEnergeticsData.txt
#' @return A nice plot
#' @export
PlotEnergetics = function(data) {
 if(!is.data.table(data))
 {
  stop(cat('You appear to have loaded your results file using read.table().\n
    please use the fread function in the package data.table\n'))
}

ys = unique(data[,Year])
ys = ys+2011  # Quick fix to match dates from field data
ysorigins = as.Date(paste0(ys, '-01-01'))
# setkey(data, 'Goose Type')
for (i in seq_along(ys)) {
  data[Year == i, Date:=as.Date(Day, origin = ysorigins[i])]
}

data[, season:=c(0, cumsum(diff(Month) > 1))]  # okay
data.table::setnames(data, old = 'Goose Type', new = 'GooseType')
data.table::setkey(data, 'GooseType')
data[, Type:='Sim']

# Read the API field data
field = data.table::fread('o:/ST_GooseProject/Field data/Weight development_all years.csv')
field[, Date:=lubridate::dmy(Date)]
field[, GooseType:='PFF']
field[, Type:='Field']
field[, c('DOY', 'SEXE'):=NULL]  # DOY is redundant and Sex is currently not used
setnames(field, old = 'Weigth', new = 'Weight')
# Currently we only use this subset:
field = field[Date > lubridate::dmy('29-08-2012') &
Date < lubridate::dmy('31-12-2013')]
field = field[lubridate::month(field$Date) < 5 | lubridate::month(field$Date) > 9,]
field[, season:=c(0, cumsum(diff(Date)/ddays(1) > 100))]
field[,season:=season+100]

data[, Date:=lubridate::ymd(Date)]
data = data[GooseType == 'PFF',.(Date, Weight, GooseType, Type)]
data[, season:=c(0, cumsum(diff(Date)/lubridate::ddays(1) > 100))]
temp = rbind(data, field)
setkey(temp, 'Date')

p = ggplot(temp[GooseType == 'PFF',], aes(Date, Weight, color = factor(Type))) +
geom_point(alpha = 1/50) + 
geom_smooth(aes(group = season)) +
theme_bw() + 
theme(axis.text=element_text(size=8))
return(p)
}