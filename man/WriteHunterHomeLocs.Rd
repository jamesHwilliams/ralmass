% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/WriteHunterHomeLocs.r
\name{WriteHunterHomeLocs}
\alias{WriteHunterHomeLocs}
\title{Write hunter home location file}
\usage{
WriteHunterHomeLocs(Locs = NULL, PathToFile = NULL)
}
\arguments{
\item{Locs}{data.frame or data.table The object with home locations}

\item{PathToFile}{character The path to the where the file is 
written (including the name and extension of the file, usually .txt)}
}
\value{
A tab separated text file formatted according to the requirements
 for a hunter home location file for ALMaSS.
}
\description{
Write a file with hunter home locations
}

