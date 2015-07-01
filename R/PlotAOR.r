#' Plot the AOR index
#'
#' Plot the AOR index (Høye et al. 2012). The index is based on the 
#' abundance–occupancy relationship and facilitates the interpretation of 
#' outputs from individual based models.
#' 
#' @param data data.frame Data.frame with x, y and scenario column.
#' @param x character The name of the column with x coordinates (occupancy).
#' @param y character The name of the column with y coordinates (abundance).
#' @param scenarios character The name of the column indicating which scenario
#' a given set of coordinates belong to.
#' @return An AOR plot
#' @references Høye, T. T., Skov, F. & Topping, C.J. (2012). Interpreting
#' outputs of agent-based models using abundance-occupancy relationships. 
#' Ecological Indicators 20: 221-227.
#' @export
#' @examples
#' df = data.frame('x' = rnorm(8, 0, .5), 'y' = rnorm(8, 0, .5), 'scenario' = letters[1:8])
#' PlotAOR(df, x = 'x', y = 'y', scenarios = 'scenario')
PlotAOR = function(data, x = NULL, y = NULL, scenarios = NULL)
{
# Append the origo point to the data:
  dataorigo = data.frame(x = rep(0, nrow(data)), y = rep(0, nrow(data)),
   'scenario' = unique(data[,scenarios]))
  dataorigo = rbind(data, dataorigo)
# Setup the plot
  p = ggplot2::ggplot(dataorigo, ggplot2::aes_string(x,y)) +
  ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_line(ggplot2::aes(color = scenario)) +
  ggplot2::geom_point(data = data, ggplot2::aes_string(x,y, color = scenarios), size = 3) +
  ggplot2::ggtitle('AOR') +
  ggplot2::labs(y = expression(paste(Delta, 'Abundance')), 
    x = expression(paste(Delta, 'Occupancy'))) +
  ggplot2::theme_bw() +
  ggplot2::ylim(-1,1) +
  ggplot2::xlim(-1,1)
  return(p)
}
