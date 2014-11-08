#' Plot results from probe0
#'
#' Plot the results from probe0
#' Note: you should read the results file using the fread function in the data.table package
#' It is very fast - you will not regret it. 
#' 
#' If the species you simulated does not use all 6 columns in the probe0.res
#' file, you should get rid of the redundant columns before calling this
#' function.
#' 
#' @param data data.table The raw output from probe0 or probe from ALMaSS
#' @param seasons logical Should breeding season and hibernation polygons be added to the plot?
#' (dormouse specific)
#' @param lty character Either 'l' = lines, 'b' both lines and point.
#' @param add Should the plot be added to an already existing plot in the plot window?
#' @param species What species is the plot for? (currenly only "Dormouse", "Goose", "Hare" available)
#' @return A nice plot
#' @export
PlotProbe0 = function(data, seasons = FALSE, lty = 'l', add = FALSE, species = 'Dormouse') {
	if(!is.data.table(data))
    { cat('You appear to have loaded your results file using read.table().\n')
      cat('please use the fread function in the package data.table')
      return()
    }
  if(species == 'Dormouse')
	{
		col = brewer.pal(4, 'Set1')
		setnames(data, c('Julian.day', 'Juvenile.male', 'Juvenile.female', 'Male', 'Female'))

		xlimits = c(data[1,1]-1, data[nrow(data),1]+1)
		ylimits = c(0, max(data[,Male], data[,Female], data[,Juvenile.male], data[,Juvenile.female]))
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
    setnames(data, c('Julian.day', 'Pinkfoot.family', 'Pinkfoot.nonbreeder', 'Barnacle.family', 'Barnacle.nonbreeder', 'Greylag.family', 'Greylag.nonbreeder'))

    xlimits = range(data[,Julian.day])+c(-1,1)
    ylimits = c(0, max(data[,Pinkfoot.family], data[,Pinkfoot.nonbreeder], data[,Barnacle.family], data[,Barnacle.nonbreeder], data[,Greylag.family], data[,Greylag.nonbreeder]))
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
      plot(Julian.day, Pinkfoot.family, type = 'n', las = 1, bty = 'l', ylim = ylimits, xlim = xlimits, ylab = 'Individuals', xlab = 'Julian day')
      # Lines
      lines(Julian.day, Pinkfoot.family, type = lty, col = gl.col, las = 1, bty = 'l')
      lines(Julian.day, Pinkfoot.nonbreeder, type = lty, col = glb.col, las = 1, bty = 'l')
      lines(Julian.day, Barnacle.family, type = lty, col = pf.col, las = 1, bty = 'l')
      lines(Julian.day, Barnacle.nonbreeder, type = lty, col = pfb.col,las = 1, bty = 'l')
      lines(Julian.day, Greylag.family, type = lty, col = b.col,las = 1, bty = 'l')
      lines(Julian.day, Greylag.nonbreeder, type = lty, col = bb.col,las = 1, bty = 'l')
      for(i in 1:nyears){
        abline(v = i*365, col = 'grey', lty = 2)
      }

      legend(0, max(ylimits)*1.15, legend = c('Pinkfoot family', 'Pinkfoot nonbreeder', 'Barnacle family', 'Barnacle nonbreeder', 'Greylag family', 'Greylag nonbreeder'),
       pch = 16, col = col, bty = 'n', cex = 0.8, xpd = NA,
       ncol = 3)
    })
}

if(species == 'Hare') 
{
    col = brewer.pal(5, 'Set1')
    setnames(data, c('Julian.day', 'Infant', 'Young', 'Juvenile', 'Male', 'Female'))

    xlimits = range(data[,Julian.day])+c(-1,1)
    ylimits = c(0, max(data[,Infant], data[,Young], data[,Juvenile], data[,Male], data[,Female]))
    xcoord = 0.8 * xlimits[2]
    ycoord =  ylimits[2]
    nyears = data[nrow(data),1]/365

    infant.col = col[5]
    young.col = col[4]
    juvenile.col = col[3]
    male.col = col[2]
    female.col = col[1]

 with(data, {
      plot(Julian.day, Juvenile, type = 'n', las = 1, bty = 'l', ylim = ylimits, xlim = xlimits, ylab = 'Individuals', xlab = 'Julian day')
      # Lines
      lines(Julian.day, Infant, type = lty, col = infant.col, las = 1, bty = 'l')
      lines(Julian.day, Young, type = lty, col = young.col, las = 1, bty = 'l')
      lines(Julian.day, Juvenile, type = lty, col = juvenile.col, las = 1, bty = 'l')
      lines(Julian.day, Male, type = lty, col = male.col, las = 1, bty = 'l')
      lines(Julian.day, Female, type = lty, col = female.col, las = 1, bty = 'l')
      for(i in 1:nyears){
        abline(v = i*365, col = 'grey', lty = 2)
      }

      legend(0, max(ylimits)*1.15, legend = c('Infant', 'Young', 'Juvenile', 'Male', 'Female'),
       pch = 16, col = col[5:1], bty = 'n', cex = 0.8, xpd = NA, horiz = TRUE)
    })
}

else {
    cat('PlotProbe0 not implemented for the species you requested - sorry!')
  }

}

