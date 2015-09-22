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

	scale = ggplot2::scale_x_continuous(breaks = seq(start, end, by = by)) 
	theme = ggplot2::theme_bw()
	data[,tovNum:=as.factor(tovNum)]
	h = ggplot2::ggplot(data, ggplot2::aes(Julian, Height)) + 
	  ggplot2::geom_line(ggplot2::aes(color = tovNum)) + scale + theme 

# Annotation to indicate grazing
	ones = data[,Grazed]
	ones[ones != 0] = 1
	if(length(ones[which(ones > 0)]) > 0) 
	{
		asdf  = ones[-1]-ones[-length(ones)]
		cows = data.frame(start = data[which(asdf == 1)+1,Julian], end = data[which(asdf == -1), Julian])
	    years = nrow(data)/365
		for (i in 1:years) {
			xrng = cows[i,]
			h = h + ggplot2::annotate('rect', xmin = xrng$start, xmax = xrng$end, ymin = -Inf, ymax = +Inf, alpha = .2) + 
			ggplot2::annotate('text', x = xrng$start + (xrng$end - xrng$start)/2, y = yrng[2]-1, label = 'grazed',  colour = 'darkgrey')
		}
	}

	gg = ggplot2::ggplot(data, ggplot2::aes(Julian, GooseGrazing)) + ggplot2::geom_line() + scale + theme
	d = ggplot2::ggplot(data, ggplot2::aes(Julian, Digestability)) + ggplot2::geom_line() + scale + theme
	db = ggplot2::ggplot(data, ggplot2::aes(Julian, DeadBiomass)) + ggplot2::geom_line() + scale + theme
	gb = ggplot2::ggplot(data, ggplot2::aes(Julian, GreenBiomass)) + ggplot2::geom_line() + scale + theme
	lat = ggplot2::ggplot(data, ggplot2::aes(Julian, LATotal)) + ggplot2::geom_line() + scale + theme
	lag = ggplot2::ggplot(data, ggplot2::aes(Julian, LAGreen)) + ggplot2::geom_line() + scale + theme
	w = ggplot2::ggplot(data, ggplot2::aes(Julian, WeedBiomass)) + ggplot2::geom_line() + scale + theme
	b = ggplot2::ggplot(data, ggplot2::aes(Julian, Biomass)) + ggplot2::geom_line() + scale + theme

	gridExtra::grid.arrange(h, gg, d, db, gb, lat, lag, w, b, nrow = 3, ncol = 3)
}