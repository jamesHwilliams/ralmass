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
#' @param drop bool Skip the first start up year (recommended and default)
#' @return A nice plot
#' @export
PlotVegDump = function(data, drop = TRUE) {
start = 0
end = 365
by = 50
if(drop) 
{
data = data[Year > 0]
}

yrng = range(data[,Height])
xrng = range(data[Grazed == 1,Day])

scale = scale_x_continuous(breaks = seq(start, end, by = by)) 
theme = theme_bw()

h = ggplot(data, aes(Day, Height)) + geom_line() + scale + theme 
h = h + annotate('rect', xmin = xrng[1], xmax = xrng[2], ymin = -Inf, ymax = +Inf, alpha = .2) + 
annotate('text', x = xrng[1] + (xrng[2] - xrng[1])/2, y = yrng[2]-1, label = 'grazed',  colour = 'darkgrey')
gg = ggplot(data, aes(Day, GooseGrazing)) + geom_line() + scale + theme
d = ggplot(data, aes(Day, Digestability)) + geom_line() + scale + theme
db = ggplot(data, aes(Day, DeadBiomass)) + geom_line() + scale + theme
gb = ggplot(data, aes(Day, GreenBiomass)) + geom_line() + scale + theme
lat = ggplot(data, aes(Day, LATotal)) + geom_line() + scale + theme
lag = ggplot(data, aes(Day, LAGreen)) + geom_line() + scale + theme
w = ggplot(data, aes(Day, WeedBiomass)) + geom_line() + scale + theme
b = ggplot(data, aes(Day, Biomass)) + geom_line() + scale + theme

grid.arrange(h, gg, d, db, gb, lat, lag, w, b, nrow = 3, ncol = 3)
}