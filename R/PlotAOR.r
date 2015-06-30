#' Plot energy output from the ALMaSS Goose Management model
#'
#' Plot the results from the energy probe
#' 
#' The energetics output can be very large, so please use the fread function 
#'  when loading the data.
#'  
#' @param data data.table The raw output from the file GooseEnergeticsData.txt
#' @param type character Currently only accepts "mean".
#' @param species character What species to plot? Defaults to "all"
#' @param package character Base- or ggplot2-type ? Defaults to "ggplot2"
#' @param scales character The argument to be used in facet_wrap if the ggplot2
#' package is used. Either free_x or free_y.
#' @return A nice plot
#' @export
# AOR hacking:

df = data.frame('x' = rnorm(8, 0, .5), 'y' = rnorm(8, 0, .5), 'scenario' = letters[1:8])
PlotAOR = function(data, x = NULL, y = NULL, scenarios = NULL)
{
	
scenarios = unique(as.character(df[,scenarios]))
col = RColorBrewer::brewer.pal(length(scenarios), 'Set2')

plot(-1:1,-1:1, type = 'n', las = 1, xlab = 'Abundance', ylab = 'Occupancy')
abline(h = 0)
abline(v = 0)

for (i in 1:nrow(df)) {
	with(df[which(df$scenario == scenarios[i]),], {
		lines(rbind(c(0,0), c(x, y)), col = col[i], lwd = 2)
		# segments(-.5,-.45, -.5, -.55, col = col[i], lwd = 1.5)
		# segments(-.45,-.5, -.55, -.5, col = col[i], lwd = 1.5)
		points(x,y, pch = 21, col = 'darkgrey', cex = 1.4, bg = col[i])
	})
}

legend(-1, 1.3, legend = scenarios, bty = 'n', horiz = TRUE, 
	xpd = NA, pch = 21, cex = 1.4, pt.bg = col, col = 'darkgrey')
}


library(ggplot2)
dforigo = data.frame(x = rep(0, nrow(df)), y = rep(0, nrow(df)), 'scenario' = unique(df$scenario))
dforigo = rbind(df, dforigo)
p = ggplot(dforigo, aes(x,y)) + geom_vline(xintercept = 0) + geom_hline(yintercept = 0)
p = p + geom_line(aes(color = scenario)) + geom_point(data = df, aes(x,y, color = scenario), size = 3)
p = p + ggtitle('AOR') + labs(y = expression(paste(Delta, 'Abundance')), x = expression(paste(Delta, 'Occupancy'))) + theme_bw() + ylim(-1,1) + xlim(-1,1)
ggslackr(p)

?geom_point
