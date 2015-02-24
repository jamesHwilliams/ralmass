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
#' @return A nice plot
#' @export
PlotEnergetics = function(data, species = 'all', package = 'ggplot2')
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
data[,SimDate:=as.Date(Day - 1, origin = as.Date(paste(Year+1989,"-01-01", sep = '')))]
data[,geesePA:= c(0, cumsum(diff(SimDate) != 1))]
data = data[,mean(Weight), by = c('Goose Type', 'SimDate', 'Year')]
setnames(data, 'V1', 'Weight')
setkey(data, 'Goose Type')

# Set global par:
par(las = 1, bty = 'l')
xlab = 'SimDay'
ylab = 'Grams'

if(species == 'all' & package == 'base') 
{
  par(mfrow = c(3,1), mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
  xlim = c(min(data[,SimDate]), max(data[,SimDate]))
  # Greylag
  years = max(data[,Year])
  plot(data['GLF', SimDate], data['GLF', Weight], type = 'n',
    xlab = xlab, ylab = ylab, main = 'Greylag', xlim = xlim, 
    ylim = c(min(data['GLF',Weight])-1, max(data['GLF',Weight])+1))
  for (i in 1:years) {
    lines(data[Year == i,]['GLF', SimDate], data[Year == i,]['GLF', Weight])
    lines(data[Year == i,]['GLNB', SimDate], data[Year == i,]['GLNB', Weight])
  }
  # Pinkfoot
  plot(data['PFF', SimDate], data['PFF', Weight], type = 'n',
    xlab = xlab, ylab = ylab, main = 'Pinkfoot', xlim = xlim, 
    ylim = c(min(data['PFF' ,Weight])-1, max(data['PFF' ,Weight])+1))
  for (i in 1:years) {
    lines(data[Year == i,]['PFF', SimDate], data[Year == i,]['PFF', Weight])
    lines(data[Year == i,]['PFNB', SimDate], data[Year == i,]['PFNB', Weight])
  }
  # Barnacle
  plot(data['BGF', SimDate], data['BGF', Weight], type = 'n',
    xlab = xlab, ylab = ylab, main = 'Barnacle', xlim = xlim, 
    ylim = c(min(data['BGF' ,Weight])-1, max(data['BGF' ,Weight])+1))
  for (i in 1:years) {
    lines(data[Year == i,]['BGF', SimDate], data[Year == i,]['BGF', Weight])
    lines(data[Year == i,]['BGNB', SimDate], data[Year == i,]['BGNB', Weight])
  }
}

if(species == 'all' & package == 'ggplot2') 
{
g = ggplot(data['GLF'], aes(SimDate, Weight)) + geom_line(aes(group = geesePA)) + facet_wrap( ~ geesePA, scales = 'free_x')
p = ggplot(data['PFF'], aes(SimDate, Weight)) + geom_line(aes(group = geesePA)) + facet_wrap( ~ geesePA, scales = 'free_x')
b = ggplot(data['BGF'], aes(SimDate, Weight)) + geom_line(aes(group = geesePA)) + facet_wrap( ~ geesePA, scales = 'free_x')
grid.arrange(g, p, b, nrow = 3, ncol = 1)
}

if(species != 'all' & package == 'base') 
{
  par(mfrow = c(1,1), mar = c(5, 5, 4, 2) + 0.1, mgp = c(4, 1, 0))
  plot(data[species, Day], data[species, Weight], xlab = xlab, ylab = ylab,
   type = 'l', lwd = 2, main = species)
}
# Reset par
par(mfrow = c(1,1), mar = c(5, 4, 4, 2) + 0.1, mgp = c(3, 1, 0))
}