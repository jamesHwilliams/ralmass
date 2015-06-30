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
PlotAOR = function(data, x = NULL, y = NULL, scenarios = NULL)
{
# Append the origo point to the data:
  dataorigo = data.frame(x = rep(0, nrow(data)), y = rep(0, nrow(data)), 'scenario' = unique(data[,scenarios]))
  dataorigo = rbind(data, dataorigo)
# Setup the plot
  p = ggplot2::ggplot(dataorigo, aes_string(x,y)) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_line(aes(color = scenario)) +
  ggplot2::geom_point(data = data, aes_string(x,y, color = scenario), size = 3) +
  ggplot2::ggtitle('AOR') +
  ggplot2::labs(y = expression(paste(Delta, 'Abundance')), x = expression(paste(Delta, 'Occupancy'))) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-1,1) +
  ggplot2::xlim(-1,1)
  return(p)
}

# Example
# Generate random data:
df = data.frame('x' = rnorm(8, 0, .5), 'y' = rnorm(8, 0, .5), 'scenario' = letters[1:8])
PlotAOR(df, x = 'x', y = 'y', scenarios = 'scenario')
