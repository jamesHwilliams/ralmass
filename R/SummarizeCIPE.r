#' Summarize output from the CIPEgrid probe
#'
#' Calculate the average density of animals in the landscape based on output
#' from the CIPEgrid probe.
#' 
#' @param CIPEgrid data.frame A dataframe in the raw format from the CIPEgrid probe. 
#' @return A data.frame with two columns. One is the timestep, the other the density
#' @export

SummarizeCIPE = function(CIPEgrid, plot = TRUE) {
	m = as.matrix(CIPEgrid)
	density = apply(m, MARGIN = 1, mean)
	out = data.frame('TimeStep' = 1:nrow(m), 'Density' = density)
	if(plot){
		plot(1:nrow(m),density, las = 1, xlab = 'Time step', ylab = 'Density', bty = 'l', pch = 21, bg = 'lightgrey', type = 'b')
	}
	return(out)
}
