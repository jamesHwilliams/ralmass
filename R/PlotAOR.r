#' Plot the AOR index
#'
#' Plot the AOR index (Hoye et al. 2012). The index is based on the 
#' abundance-occupancy relationship and facilitates the interpretation of 
#' outputs from individual based models.
#' 
#' @param data data.frame Data.frame with x, y and scenario column.
#' @param x character The name of the column with x coordinates (occupancy).
#' @param y character The name of the column with y coordinates (abundance).
#' @param scenarios character The name of the column indicating which scenario
#' a given set of coordinates belong to.
#' @param title character The title of the plot (optional).
#' @param fixed logical If TRUE (the default) plot has xlim and ylim -1,1.
#' @param triangle logical Should the area of decrease on both axes be highlighted?
#' default is FALSE.
#' @param trigger numeric The trigger value for accepted decrease in the
#' summed-AOR indexs score. Mostly used when assessing impact of pesticides.
#' Any points falling beyond the area highlighted by the shaded triangle are an
#' unacceptable level of impact.
#' @return An AOR plot
#' @references Hoye, T. T., Skov, F. & Topping, C.J. (2012). Interpreting
#' outputs of agent-based models using abundance-occupancy relationships. 
#' Ecological Indicators 20: 221-227.
#' @export
#' @examples
#' df = data.frame('x' = rnorm(8, 0, .5), 'y' = rnorm(8, 0, .5), 'scenario' = letters[1:8])
#' PlotAOR(df, x = 'x', y = 'y', scenarios = 'scenario')
PlotAOR = function(data, x = NULL, y = NULL, scenarios = NULL, fixed = TRUE,
 title = NULL, legendtitle = NULL, triangle = FALSE, trigger = 0, brewerpal = NULL)
{
  # Append the origo point to the data:
  if(data.table::is.data.table(data))
  {
    data = data[,c(x, y, scenarios), with = FALSE]
    dataorigo = data.frame(x = rep(0, nrow(data)), y = rep(0, nrow(data)),
     'scenario' = unique(data[,scenarios, with = FALSE]))
    setnames(dataorigo, c(x,y,scenarios))
  }
  if(!data.table::is.data.table(data))
  {
    data = data[,match(c(x, y, scenarios), names(data))]
    dataorigo = data.frame(x = rep(0, nrow(data)), y = rep(0, nrow(data)),
     'scenario' = unique(data[,scenarios]))
    names(dataorigo) = c(x,y,scenarios)
  }
  dataorigo = rbind(data, dataorigo)
  ltitle = ''
  if(!is.null(legendtitle))
  {
    ltitle = legendtitle
  }
# Setup the plot
  p = ggplot2::ggplot(dataorigo, ggplot2::aes_string(x,y))
if(!is.null(brewerpal))
{
  p = p + ggplot2::scale_colour_brewer(palette=brewerpal, name = ltitle)
}
  if(triangle)
  {
    trigger = abs(trigger)
    triangle = data.frame(x = c(-1,-1,1-trigger), y = c(1-trigger,-1,-1))
    p = p + ggplot2::geom_polygon(data = triangle, ggplot2::aes(x=x, y=y), alpha = 0.05)
  }
  p = p + ggplot2::geom_vline(xintercept = 0) +
  ggplot2::geom_hline(yintercept = 0) +
  ggplot2::geom_line(ggplot2::aes_string(color = scenarios)) +
  ggplot2::geom_point(data = data, ggplot2::aes_string(x,y, color = scenarios), size = 3) +
  ggplot2::labs(y = expression(paste(Delta, ' Abundance')), 
    x = expression(paste(Delta, ' Occupancy'))) +
  ggplot2::theme_bw()
  if(!is.null(title))
  {
    p = p + ggplot2::ggtitle(title)
  }
  if(fixed)
  {
   p = p + ggplot2::coord_cartesian(ylim = c(-1,1), xlim = c(-1,1))
   return(p)
 }
 if(!fixed)
 {
  return(p)
}
}
