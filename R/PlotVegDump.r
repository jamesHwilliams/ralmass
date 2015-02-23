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
	end = nrow(data)
# Initial year is an internal set-up year - normally discarded
	if(drop) 
	{
		data = data[Year > 0,]
		end = nrow(data)
	}
# Adjust tick mark and label spacing
	if(end >= 730) 
	{
		by = 100
	}
	if(end < 730) 
	{
		by = 50
	}

	data[,Julian:=1:end]

	yrng = range(data[,Height])
# xrng = range(data[Grazed == 1,Julian])

	scale = scale_x_continuous(breaks = seq(start, end, by = by)) 
	theme = theme_bw()

	h = ggplot(data, aes(Julian, Height)) + geom_line() + scale + theme 

# Annotation to indicate grazing
	ones = data[,Grazed]
	if(length(ones[which(ones > 0)]) > 0) 
	{
		asdf  = ones[-1]-ones[-length(ones)]
		cows = data.frame(start = data[which(asdf == 1)+1,Julian], end = data[which(asdf == -1), Julian])
	    years = nrow(data)/365
		for (i in 1:years) {
			xrng = cows[i,]
			h = h + annotate('rect', xmin = xrng$start, xmax = xrng$end, ymin = -Inf, ymax = +Inf, alpha = .2) + 
			annotate('text', x = xrng$start + (xrng$end - xrng$start)/2, y = yrng[2]-1, label = 'grazed',  colour = 'darkgrey')
		}
	}

	gg = ggplot(data, aes(Julian, GooseGrazing)) + geom_line() + scale + theme
	d = ggplot(data, aes(Julian, Digestability)) + geom_line() + scale + theme
	db = ggplot(data, aes(Julian, DeadBiomass)) + geom_line() + scale + theme
	gb = ggplot(data, aes(Julian, GreenBiomass)) + geom_line() + scale + theme
	lat = ggplot(data, aes(Julian, LATotal)) + geom_line() + scale + theme
	lag = ggplot(data, aes(Julian, LAGreen)) + geom_line() + scale + theme
	w = ggplot(data, aes(Julian, WeedBiomass)) + geom_line() + scale + theme
	b = ggplot(data, aes(Julian, Biomass)) + geom_line() + scale + theme

	grid.arrange(h, gg, d, db, gb, lat, lag, w, b, nrow = 3, ncol = 3)
}