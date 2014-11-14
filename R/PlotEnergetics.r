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
#' @return A nice plot
#' @export
PlotEnergetics = function(data, species = 'all')
{
 if(!is.data.table(data))
    {
     cat('You appear to have loaded your results file using read.table().\n')
     cat('please use the fread function in the package data.table')
     return()
    }
    if(!species %in% c('all', 'PFF', 'PFNB', 'BGF', 'BGNB', 'GLF', 'GLNB'))
    {
      cat('\n')
      cat('Something is not right with the species argument.\n')
      cat('Please provide one of these: \n')
      cat('all', 'PFF', 'PFNB', 'BGF', 'BGNB', 'GLF', 'GLNB\n')
      return()
    }

  setkey(data, 'Goose Type')
  data = data[,mean(Weight),by=c('Goose Type', 'Day')]
  setnames(data, 'V1', 'Weight')

# Set global par:
  par(las = 1, bty = 'l')
  xlab = 'Day'
  ylab = 'Grams'
  
  if(species == 'all') 
  {
    par(mfrow = c(3,1), mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
    xlim = c(min(data[,Day]), max(data[,Day]))
  # Greylag
    plot(data['GLF', Day], data['GLF', Weight], type = 'n',
      xlab = xlab, ylab = ylab, main = 'Greylag', xlim = xlim, 
      ylim = c(min(data['GLF',Weight])-1, max(data['GLF',Weight])+1))
    lines(data['GLF', Day], data['GLF', Weight])
    lines(data['GLF', Day], data['GLNB', Weight])
  # Pinkfoot
    plot(data['PFF', Day], data['PFF', Weight], type = 'n',
      xlab = xlab, ylab = ylab, main = 'Pinkfoot', xlim = xlim, 
      ylim = c(min(data['PFF' ,Weight])-1, max(data['PFF' ,Weight])+1))
    lines(data['PFF', Day], data['PFF', Weight])
    lines(data['PFF', Day], data['PFNB', Weight])
  # Barnacle
    plot(data['BGF', Day], data['BGF', Weight], type = 'n',
      xlab = xlab, ylab = ylab, main = 'Barnacle', xlim = xlim, 
      ylim = c(min(data['BGF' ,Weight])-1, max(data['BGF' ,Weight])+1))
    lines(data['BGF', Day], data['BGF', Weight])
    lines(data['BGF', Day], data['BGNB', Weight])
  }

if(species != 'all') 
{
  par(mfrow = c(1,1), mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
  plot(data[species, Day], data[species, Weight], xlab = xlab, ylab = ylab,
   type = 'l', lwd = 2, main = species)
}
# Reset par
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
}

