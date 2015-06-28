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
plot(-1:1,-1:1, type = 'n', las = 1, xlab = '', ylab = '')
abline(h = 0)
abline(v = 0)
lines(rbind(c(0,0), c(-.5, -.5)), col = 'Steelblue1', lwd = 2)
# segments(0,0, -.5, -.5, col = 'green', lwd = 3)
segments(-.5,-.45, -.5, -.55, col = 'Steelblue1', lwd = 1.5)
segments(-.45,-.5, -.55, -.5, col = 'Steelblue1', lwd = 1.5)
points(-.5,-.5, pch = 21, col = 'darkblue', cex = 1.5, bg = 'Steelblue1')
