#' Plot results from the TerritoryProbe
#'
#' Plot the results from the TerritoryProbe (currently only implemented in the Dormouse code) 
#' 
#' @param data data.frame The raw output from the TerritoryProbe or probe from ALMaSS
#' @param seasons logical Should breeding season and hibernation polygons be added to the plot? 
#' @param type character Either 'l' = lines, 'b' = both lines and point.
#' @return A nice plot
#' @export
PlotTerritoryProbe = function(data, seasons = TRUE, type = 'l') {
  col = RColorBrewer::brewer.pal(4, 'Set1')
  summarized.data = plyr::ddply(data, .(Year, SimDay, Sex), summarize, Proportion  = (length(which(Territory == 1))/length(Territory)))

  xlimits = c(min(data$SimDay)-1, 1+max(data$SimDay))
  ylimits = c(0,1)
  xcoord = min(data$SimDay)
  ycoord =  ylimits[2]
  nyears = max(data$Year)

  m.col = col[2]
  f.col = col[1]
  jm.col = col[3]
  jf.col = col[4]

  m.pch = 15
  f.pch = 19

  BreedingSeaStart = 120
  BreedingSeaEnd = 243
  HibernationStart = 300
  HibernationEnd = 100

  with(summarized.data, {
    plot(SimDay, Proportion, type = 'n', las = 1, bty = 'l', ylim = ylimits, xlim = xlimits, ylab = 'Proportion holding territory', xlab = 'Julian day')
    if(seasons){
      for (i in 1:nyears)
      {
        if(i == 1){
          rect(xleft = BreedingSeaStart, xright = BreedingSeaEnd, ybottom = min(ylimits), ytop = max(ylimits), col = 'grey95', border = "transparent")
        }
        if(i > 1){
          rect(xleft = BreedingSeaStart+(365*(i-1)), xright = BreedingSeaEnd+(365*(i-1)), ybottom = min(ylimits), ytop = max(ylimits), col = 'grey95', border = "transparent")
        }
      }
  # Hibernation polygons:
      rect(xleft = 0, xright = HibernationEnd, ybottom = min(ylimits), ytop = max(ylimits), col = 'grey90', density = 10, border = "transparent")
      if(nyears == 1){
        rect(xleft = HibernationStart, xright = max(xlimits), ybottom = min(ylimits), ytop = max(ylimits), col = 'grey90', density = 10, border = "transparent")
      }
      if(nyears > 1) {
        for (i in 1:nyears)
        {
          rect(xleft = HibernationStart+(365*(i-1)), xright = HibernationEnd+(365*(i+1-1)), ybottom = min(ylimits), ytop = max(ylimits), col = 'grey95', density = 10, border = "transparent")
        }
      }
    }
    lines(SimDay[which(Sex == 'F')], Proportion[which(Sex == 'F')], type = type, col = f.col, pch = f.pch)
    lines(SimDay[which(Sex == 'M')], Proportion[which(Sex == 'M')], type = type, col = m.col, pch = m.pch)

    for(i in 1:nyears){
      abline(v = i*365, col = 'lightgrey', lty = 2)
    }

    legend(0, max(ylimits)*1.1, legend = c('Male','Female'), pch=c(m.pch, f.pch), col=c(m.col, f.col), bty = 'n', cex = 0.8,
     xpd = NA, text.width = c(strwidth('Male'), strwidth('Female')), horiz = TRUE)
  })
}


