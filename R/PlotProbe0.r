#' Plot results from probe0
#'
#' Plot the results from probe0 
#' 
#' @param data data.frame The raw output from probe0 or probe from ALMaSS
#' @param seasons logical Should breeding season and hibernation polygons be added to the plot? 
#' @param lty character Either 'l' = lines, 'b' both lines and point.
#' @return A nice plot
#' @export
PlotProbe0 = function(data, seasons = TRUE, lty = 'l', add = FALSE, species = 'Dormouse') {
	if(species == 'Dormouse')
	{
		col = brewer.pal(4, 'Set1')
		names(data) = c('Julian.day', 'Juvenile.male', 'Juvenile.female', 'Male', 'Female')

		xlimits = c(data[1,1]-1, data[nrow(data),1]+1)
		ylimits = c(0, max(data[,'Male'], data[,'Female'], data[,'Juvenile.male'], data[,'Juvenile.female']))
		xcoord = 0.8 * xlimits[2]
		ycoord =  ylimits[2]
		nyears = data[nrow(data),1]/365

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

  if(!add) {
    with(data, {
      plot(Julian.day, Male, type = 'n', las = 1, bty = 'l', ylim = ylimits, xlim = xlimits, ylab = 'Individuals', xlab = 'Julian day')
      if(seasons){
  # Breeding season polygons:
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
  # Lines
      lines(Julian.day, Male, type = lty, col = m.col, las = 1, bty = 'l', pch = m.pch)
      lines(Julian.day, Female, type = lty, col = f.col, las = 1, bty = 'l', pch = f.pch)
      lines(Julian.day, Juvenile.male, type = lty, col = jm.col, las = 1, bty = 'l', pch = m.pch)
      lines(Julian.day, Juvenile.female, type = lty, col = jf.col,las = 1, bty = 'l', pch = f.pch)
      for(i in 1:nyears){
        abline(v = i*365, col = 'grey', lty = 2)
      }

      legend(0, max(ylimits)*1.15, legend = c('Male','Female', 'Juv. male', 'Juv. female'),
       pch = c(m.pch, f.pch, m.pch, f.pch), col = c(m.col, f.col, jm.col, jf.col), bty = 'n',
       cex = 0.8, xpd = NA, text.width = c(strwidth('Male'), strwidth('Female'), strwidth('Juv. male'), strwidth('Juv. female')),
       horiz = TRUE)
    })
}

if(add) {
  with(data, {
    # Lines
    lines(Julian.day, Male, type = lty, col = m.col, las = 1, bty = 'l', pch = m.pch)
    lines(Julian.day, Female, type = lty, col = f.col, las = 1, bty = 'l', pch = f.pch)
    lines(Julian.day, Juvenile.male, type = lty, col = jm.col, las = 1, bty = 'l', pch = m.pch)
    lines(Julian.day, Juvenile.female, type = lty, col = jf.col,las = 1, bty = 'l', pch = f.pch)
  })
}
}

if(species == 'Goose') {
    col = c(brewer.pal(9, 'Blues')[c(8, 6)], brewer.pal(8, 'Greens')[c(8, 6)], brewer.pal(8, 'Reds')[c(8, 6)])
    setnames(data, c('Julian.day', 'Greylag', 'GreylagB', 'PinkFeet', 'PinkFeetB', 'Barny', 'BarnyB'))

    xlimits = range(data[,Julian.day])+c(-1,1)
    ylimits = c(0, max(data[,Greylag], data[,GreylagB], data[,PinkFeet], data[,PinkFeetB], data[,Barny], data[,BarnyB]))
    xcoord = 0.8 * xlimits[2]
    ycoord =  ylimits[2]
    nyears = data[nrow(data),1]/365

    gl.col = col[1]
    glb.col = col[2]
    pf.col = col[3]
    pfb.col = col[4]
    b.col = col[5]
    bb.col = col[6]

 with(data, {
      plot(Julian.day, Greylag, type = 'n', las = 1, bty = 'l', ylim = ylimits, xlim = xlimits, ylab = 'Individuals', xlab = 'Julian day')
      # Lines
      lines(Julian.day, Greylag, type = lty, col = gl.col, las = 1, bty = 'l')
      lines(Julian.day, GreylagB, type = lty, col = glb.col, las = 1, bty = 'l')
      lines(Julian.day, PinkFeet, type = lty, col = pf.col, las = 1, bty = 'l')
      lines(Julian.day, PinkFeetB, type = lty, col = pfb.col,las = 1, bty = 'l')
      lines(Julian.day, Barny, type = lty, col = b.col,las = 1, bty = 'l')
      lines(Julian.day, BarnyB, type = lty, col = bb.col,las = 1, bty = 'l')
      for(i in 1:nyears){
        abline(v = i*365, col = 'grey', lty = 2)
      }

      legend(0, max(ylimits)*1.15, legend = c('Greylag', 'GreylagB', 'PinkFeet', 'PinkFeetB', 'Barny', 'BarnyB'),
       pch = 16, col = col, bty = 'n', cex = 0.8, xpd = NA,
       #text.width = c(strwidth('Greylag'), strwidth('GreylagB'), strwidth('PinkFeet'), strwidth('PinkFeetB'), strwidth('Barny'), strwidth('BarnyB')),
       horiz = TRUE)
    })
}

else {
    cat('PlotPrbe0 not implemented for the species you requested - sorry!')
  }

}

