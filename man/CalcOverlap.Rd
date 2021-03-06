% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CalcOverlap.r
\name{CalcOverlap}
\alias{CalcOverlap}
\title{Calculate the overlap of kernel densities}
\usage{
CalcOverlap(data = NULL, species = NULL, metric = NULL)
}
\arguments{
\item{data}{data.table An object containing both the simulated and observed 
flock sizes and the three columns 'Species', 'Numbers' and 'Type'}

\item{species}{character The species for which the calculated for.
Either 'Barnacle', 'Pinkfoot','Greylag' or 'Hunter'}

\item{metric}{character The name of the column with the metric on which 
to calculate the kernel density overlap}
}
\value{
numeric The overlap
}
\description{
Calculate the overlap of kernel densities. Either densities of flock sizes
calculated from field observations and from the simulated flock sizes, of 
hunter densities on farms or kernel densities of number of hunters per farm
}
\details{
This function is based on the answer to this CV question: 
http://stats.stackexchange.com/questions/97596/how-to-calculate-overlap
-between-empirical-probability-densities
}

