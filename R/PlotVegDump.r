#' Plot ouput from VegDump
#'
#' Plot ouput from VegDump.txt
#' 
#' The VegDump probe output various parameters from the landscape
#' Look in the actual file to see what is currently implemented
#'  
#' Note: you should read the results file using the fread function in the data.table package
#' It is very fast - you will not regret it. 
#' 
#' @param data data.table The raw VegDump.txt from ALMaSS
#' @return A nice plot
#' @export
PlotVegDump = function(data) {
start = 0
end = 365
by = 25

yrng = range(data[Year>0,Height])
xrng = range(data[Year>0 & Grazed == 1,Day])

scale = scale_x_continuous(breaks = seq(start, end, by = by)) 

h = ggplot(data[Year>0,], aes(Day, Height)) + geom_line() + scale 
h = h + annotate('rect', xmin = xrng[1], xmax = xrng[2], ymin = -Inf, ymax = +Inf, alpha = .2) + 
annotate('text', x = xrng[1] + (xrng[2] - xrng[1])/2, y = yrng[2]-1, label = 'grazed',  colour = 'darkgrey')
gg = ggplot(data[Year>0,], aes(Day, GooseGrazing)) + geom_line() + scale
d = ggplot(data[Year>0,], aes(Day, Digestability)) + geom_line() + scale
db = ggplot(data[Year>0,], aes(Day, DeadBiomass)) + geom_line() + scale
gb = ggplot(data[Year>0,], aes(Day, GreenBiomass)) + geom_line() + scale
lat = ggplot(data[Year>0,], aes(Day, LATotal)) + geom_line() + scale
lag = ggplot(data[Year>0,], aes(Day, LAGreen)) + geom_line() + scale
w = ggplot(data[Year>0,], aes(Day, WeedBiomass)) + geom_line() + scale
b = ggplot(data[Year>0,], aes(Day, Biomass)) + geom_line() + scale

grid.arrange(h, gg, d, db, gb, lat, lag, w, b, nrow = 3, ncol = 3)
}