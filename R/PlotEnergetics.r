#' Plot energy output from the ALMaSS Goose Management model
#'
#' Plot the results from the energy probe
#' 
#'  
#' @param data data.table The raw output from the file GooseEnergeticsData.txt
#' @param type character Currently only accepts "mean".
#' @param species character What species to plot? Defaults to "all"
#' @return A nice plot
#' @export
PlotEnergetics = function(data, type = 'mean', species = 'all')
{
  setkey(data, 'Goose Type')

  if(type == 'mean') 
  {
    data = data[,mean(Weight),by=c('Goose Type', 'Day')]
    setnames(data, 'V1', 'Weight')
  }
  if(species == 'all') 
  {
    par(mfrow = c(3,1), mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
    xlim = c(min(data[,Day]), max(data[,Day]))
  # Greylag
    plot(data['GLF', Day], data['GLF', Weight], type = 'n', bty = 'l', las = 1,
      xlab = 'Day', ylab = 'Grams', main = 'Greylag', xlim = xlim, 
      ylim = c(min(data['GLF',Weight])-1, max(data['GLF',Weight])+1))
    lines(data['GLF', Day], data['GLF', Weight])
    lines(data['GLF', Day], data['GLNB', Weight])
  # Pinkfoot
    plot(data['PFF', Day], data['PFF', Weight], type = 'n', bty = 'l', las = 1,
      xlab = 'Day', ylab = 'Grams', main = 'Pinkfoot', xlim = xlim, 
      ylim = c(min(data['PFF' ,Weight])-1, max(data['PFF' ,Weight])+1))
    lines(data['PFF', Day], data['PFF', Weight])
    lines(data['PFF', Day], data['PFNB', Weight])
  # Barnacle
    plot(data['BGF', Day], data['BGF', Weight], type = 'n', bty = 'l', las = 1,
      xlab = 'Day', ylab = 'Grams', main = 'Barnacle', xlim = xlim, 
      ylim = c(min(data['BGF' ,Weight])-1, max(data['BGF' ,Weight])+1))
    lines(data['BGF', Day], data['BGF', Weight])
    lines(data['BGF', Day], data['BGNB', Weight])
    par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
  }
}

